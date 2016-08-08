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

    val tpe = weakTypeOf[A]
    val companion = tpe.typeSymbol.companion

    val fields = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get.paramLists.head

    val fromDBObject: List[c.universe.Tree] = List()/*fields.map { field ⇒
      val name = field.name.toTermName
      val decoded = name.decodedName.toString
      //val returnType: c.universe.Type = tpe.decl(name).typeSignature
      //println(name,returnType,field.typeSignature)
      val tpe: c.universe.Type = field.typeSignature

      if (tpe =:= NullTpe)                                                   {
        q"$decoded ->  BsonNull()"
      } else if(tpe =:= CharTpe    || tpe =:= c.typeOf[java.lang.Character] || tpe =:= c.typeOf[java.lang.String] ) {
        q"$decoded ->  BsonString(o.$name.toString)"
      } else if(tpe =:= ByteTpe    || tpe =:= c.typeOf[java.lang.Byte     ]
        || tpe =:= ShortTpe   || tpe =:= c.typeOf[java.lang.Short    ]
        || tpe =:= IntTpe     || tpe =:= c.typeOf[java.lang.Integer  ]
        || tpe =:= LongTpe    || tpe =:= c.typeOf[java.lang.Long     ]
        || tpe =:= FloatTpe   || tpe =:= c.typeOf[java.lang.Float    ]
        || tpe =:= DoubleTpe  || tpe =:= c.typeOf[java.lang.Double   ]
        || tpe =:= BooleanTpe || tpe =:= c.typeOf[java.lang.Boolean  ]) {
        q"$decoded ->  BsonNumber(o.$name)"
      } else {
        throw new Exception(s"tpe.typeSymbol.fullName=${tpe.typeSymbol.fullName}")
      }
      // q"$decoded → t.$name"

    }*/

    val ret = c.Expr[Reads[A]] {
      q"""
         new _root_.mondao.Reads[$tpe] {
            def reads(o:BsonValue) = try {
              if ( o.isInstanceOf[BsonDocument] ) throw new _root_.mondao.MondaoException("init","case class is not BsonDocument")
              _root_.scala.util.Success($companion(..$fromDBObject))
            } catch {
              case ex:Throwable => _root_.scala.util.Failure(ex)
            }
      }
    """
    }
    println("RET",ret)
    ret
  }



  def writesImpl[A: c.WeakTypeTag](c: Context): c.Expr[Writes[A]] = {
    import c.universe._
    import definitions._

    def packOne(name:c.universe.TermName,decoded:String,tpe:c.universe.Type) = {
      println("toDBE",tpe,tpe =:= ByteTpe)

      if (tpe =:= NullTpe)                                                   {
        q"$decoded ->  BsonNull()"
      } else if(tpe =:= CharTpe    || tpe =:= c.typeOf[java.lang.Character] || tpe =:= c.typeOf[java.lang.String] ) {
        q"$decoded ->  BsonString(o.$name.toString)"
      } else if(tpe =:= ByteTpe    || tpe =:= c.typeOf[java.lang.Byte     ]
        || tpe =:= ShortTpe   || tpe =:= c.typeOf[java.lang.Short    ]
        || tpe =:= IntTpe     || tpe =:= c.typeOf[java.lang.Integer  ]
        || tpe =:= LongTpe    || tpe =:= c.typeOf[java.lang.Long     ]
        || tpe =:= FloatTpe   || tpe =:= c.typeOf[java.lang.Float    ]
        || tpe =:= DoubleTpe  || tpe =:= c.typeOf[java.lang.Double   ]
        || tpe =:= BooleanTpe || tpe =:= c.typeOf[java.lang.Boolean  ]) {
        q"$decoded ->  BsonNumber(o.$name)"
      } else c.inferImplicitValue(typeOf[Writes[tpe.type]]) match {
         case EmptyTree => {
           println(c.inferImplicitValue(typeOf[Writes[tpe.type]]))
           println(c.inferImplicitValue(typeOf[Writes[tpe.type]]))
           throw new Exception(s"tpe.typeSymbol.fullName=${tpe.typeSymbol.fullName}")
         }
         case packer => q"$decoded ->  toBson(o.$name)"
      }
    }

    val tpe: c.universe.Type = weakTypeOf[A]

    val fields = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get.paramLists.head

    val toDBObject = fields.map { field ⇒
      val name: c.universe.TermName = field.name.toTermName
      packOne(name,name.decodedName.toString,field.typeSignature)
    }

    println("reto",toDBObject)
    c.Expr[Writes[A]] {
      q"""
         import mondao.Convert.toBson
         new Writes[$tpe] {
             def writes(o:$tpe) = BsonDocument(
               ..$toDBObject
             )
      }
    """
    }

  }

}