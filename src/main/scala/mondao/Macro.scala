package mondao

import scala.annotation.implicitNotFound
import org.mongodb.scala.bson._

import scala.util.Try

@implicitNotFound(  "No serializer found for type ${A}. Try to implement an implicit Writes for this type.")
trait Writes[-A] {
  def writes(o: A): BsonValue
}

@implicitNotFound("No deserializer found for type ${A}. Try to implement an implicit Reads for this type.")
trait Reads[A] {
  def reads(o: BsonValue): Try[A]
}

object Convert {
  def toBson[T](o: T)(implicit tjs: Writes[T]): BsonValue = tjs.writes(o)
  def fromBson[T](bs: BsonValue)(implicit fjs: Reads[T]): Try[T] = fjs.reads(bs)
}

class ConvertException(where: String, s: String) extends RuntimeException(s"$where: $s")


object Macros {

  import language.experimental.macros

  def writes[A] = macro writesImpl[A]

  def reads[A] = macro readsImpl[A]

  import scala.reflect.macros.Context

  def readsImpl[A: c.WeakTypeTag](c: Context): c.Expr[Reads[A]] = {
    import c.universe._
    import definitions._

    // "a.b.c" -> Select(Select(Ident("a"), "b"), "c")
    def nameAsTree(m: String): Tree =
      m.split("\\.this\\.") match {
        case Array(t, n) => n.split('.').foldLeft[Tree](This(newTypeName(t))) { Select(_, _) }
        case Array(n)    => n.split('.').foldLeft[Tree](null) {
          case (null, part  ) => Ident(TermName(part))
          case (tre,  part  ) => Select(tre, TermName(part))
        }
      }

    def unpackOne(name:String,tpe:c.universe.Type): c.universe.Tree = {
      val oname = nameAsTree(name)

      if(tpe =:= ByteTpe    || tpe =:= c.typeOf[java.lang.Byte     ]
        || tpe =:= ShortTpe   || tpe =:= c.typeOf[java.lang.Short    ]
        || tpe =:= IntTpe     || tpe =:= c.typeOf[java.lang.Integer  ]
        || tpe =:= LongTpe    || tpe =:= c.typeOf[java.lang.Long     ]
        || tpe =:= FloatTpe   || tpe =:= c.typeOf[java.lang.Float    ]
        || tpe =:= DoubleTpe  || tpe =:= c.typeOf[java.lang.Double   ]
        || tpe =:= BooleanTpe || tpe =:= c.typeOf[java.lang.Boolean  ]) {
        q"$oname.asNumber()"
      } else {
        throw new ConvertException("unpackOne","")
      }
    }

    val tpe = weakTypeOf[A]
    val companion = tpe.typeSymbol.fullName

    val fields = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get.paramLists.head

    val fromDBObject: List[c.universe.Tree] = fields.map { field ⇒
      val name: c.universe.TermName = field.name.toTermName
      unpackOne("o." + name.decodedName.toString,field.typeSignature)
    }

    val ret = c.Expr[Reads[A]] {
      q"""
         new _root_.mondao.Reads[$tpe] {
            def reads(o:BsonValue) = try {
              if ( o.isInstanceOf[BsonDocument] ) throw new _root_.mondao.ConvertException("init","case class is not BsonDocument")
              _root_.scala.util.Success($companion(..$fromDBObject))
            } catch {
              case ex:Throwable => _root_.scala.util.Failure(ex)
            }
      }
    """
    }

    println("Replaced macro for ",weakTypeOf[A])
    println(ret)
    println("--------------------------")
    ret
  }



  def writesImpl[A: c.WeakTypeTag](c: Context): c.Expr[Writes[A]] = {
    import c.universe._
    import definitions._

    // "a.b.c" -> Select(Select(Ident("a"), "b"), "c")
    def nameAsTree(m: String): Tree =
      m.split("\\.this\\.") match {
        case Array(t, n) => n.split('.').foldLeft[Tree](This(newTypeName(t))) { Select(_, _) }
        case Array(n)    => n.split('.').foldLeft[Tree](null) {
          case (null, part  ) => Ident(TermName(part))
          case (tre,  part  ) => Select(tre, TermName(part))
        }
      }


    def packOne(name:String,tpe:c.universe.Type): c.universe.Tree = {
      val oname = nameAsTree(name)
      if (tpe =:= NullTpe)                                                   {
        q"BsonNull()"
      } else if(tpe =:= CharTpe    || tpe =:= c.typeOf[java.lang.Character] || tpe =:= c.typeOf[java.lang.String] ) {
        q"BsonString($oname.toString)"
      } else if(tpe =:= BooleanTpe) {
        q"BsonBoolean($oname)"
      } else if(tpe =:= ByteTpe    || tpe =:= c.typeOf[java.lang.Byte     ]
        || tpe =:= ShortTpe   || tpe =:= c.typeOf[java.lang.Short    ]
        || tpe =:= IntTpe     || tpe =:= c.typeOf[java.lang.Integer  ]
        || tpe =:= LongTpe    || tpe =:= c.typeOf[java.lang.Long     ]
        || tpe =:= FloatTpe   || tpe =:= c.typeOf[java.lang.Float    ]
        || tpe =:= DoubleTpe  || tpe =:= c.typeOf[java.lang.Double   ]
        || tpe =:= BooleanTpe || tpe =:= c.typeOf[java.lang.Boolean  ]) {
        q"BsonNumber($oname)"
      } else if (tpe.baseClasses.exists(_.fullName == "scala.collection.GenMapLike")) {
        val tmp = TermName(c.freshName("x$"))
        val innerType = tpe.typeArgs.tail.head
        val keyType = tpe.typeArgs.head
        val packAst = packOne("x._2",innerType)
        val packFun = q"def $tmp(x:($keyType,$innerType)) = { (x._1.toString , $packAst) }"

        q"BsonDocument({ $packFun ; $oname.map($tmp) })"

      } else if (tpe.typeSymbol.fullName == "scala.Option") {
        val tmp = TermName(c.freshName("x$"))
        val innerType = tpe.typeArgs.head
        val packAst = packOne("x",innerType)
        val packFun = q"def $tmp(x:$innerType) = { $packAst }"

        q"""{
            $packFun
            if ($oname.isDefined) $tmp($oname.get) else BsonNull()

        }"""
      } else if (tpe.typeSymbol.fullName == "scala.Array" || tpe.baseClasses.exists(_.fullName == "scala.collection.Traversable")) { // Array is final class
        val tmp = TermName(c.freshName("x$"))
        val innerType = tpe.typeArgs.head
        val packAst = packOne("x",innerType)

        val packFun = q"def $tmp(x:$innerType) = { $packAst }"
        q"BsonArray({ $packFun ; $oname.map($tmp) })"
      } else
        q"toBson($name)"
    }

    val tpe: c.universe.Type = weakTypeOf[A]

    val fields = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get.paramLists.head

    val toDBObject = fields.map { field ⇒
      val name: c.universe.TermName = field.name.toTermName
      val rightPart = packOne("o." + name.decodedName.toString,field.typeSignature)
      q"${name.decodedName.toString} -> $rightPart"
    }


    var ret =c.Expr[Writes[A]] {
      q"""
         import mondao.Convert.toBson
         new Writes[$tpe] {
             def writes(o:$tpe) = BsonDocument(
               ..$toDBObject
             )
      }
    """
    }

    println("Replaced macro for ",weakTypeOf[A])
    println(ret)
    println("--------------------------")
    ret
  }

}