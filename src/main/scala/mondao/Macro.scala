package mondao

import org.bson.BsonValue

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
  def writesDebug[A] = macro writesImplDebug[A]

  def reads[A] = macro readsImpl[A]
  def readsDebug[A] = macro readsImplDebug[A]

  import scala.reflect.macros.Context

  def readsImpl[A: c.WeakTypeTag](c: Context) = _readsImpl[A](c,false)
  def readsImplDebug[A: c.WeakTypeTag](c: Context) = _readsImpl[A](c,true)

  def _readsImpl[A: c.WeakTypeTag](c: Context,dbg:Boolean): c.Expr[Reads[A]] = {
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
/*
    def getCompanionSymbol(s: Symbol): Symbol = s.companion match {
      case NoSymbol if s.owner.isMethod || s.owner.isTerm =>
        val tn = s.name.toTermName
        def findInContext(ctx: scala.tools.nsc.typechecker.Contexts#Context): Symbol =
          ctx.scope.asInstanceOf[Scope] find { sym => sym.isModule && sym.owner == s.owner && sym.name.toTermName == tn } match {
            case Some(sym)                => assert(!sym.isMethod); sym
            case None if ctx.outer != ctx => findInContext(ctx.outer)
            case None                     => NoSymbol
          }
        findInContext(c.asInstanceOf[scala.reflect.macros.runtime.Context].callsiteTyper.context)

      case sym: Symbol => sym
    }
*/
    def unpackOne(term:String,field:String,tpe:c.universe.Type,defaultVal : Option[c.universe.Tree]): c.universe.Tree = {

      val defaultEx: c.universe.Tree =  q"""throw new _root_.mondao.ConvertException($field,"is null or not exists")"""

      val exOrDefault = defaultVal.getOrElse(defaultEx)

      val oname = if (field  == "" ) nameAsTree(term) else
        q"""{
           val t=${ nameAsTree(term) }.get($field);
           if  ( t == null ) $defaultEx else t
           }"""

      def onameWithSelector(selector:c.universe.Tree) = if (field  == "" )
        q"""{
            val t=${nameAsTree(term)}
            if  ( t == null ) $exOrDefault else $selector
                }"""else
        q"""{
           val t=${ nameAsTree(term) }.get($field);
           if  ( t == null ) $exOrDefault else $selector
           }"""

  if(
        //tpe =:= ByteTpe    || tpe =:= c.typeOf[java.lang.Byte     ]
        //|| tpe =:= ShortTpe   || tpe =:= c.typeOf[java.lang.Short    ]
         tpe =:= IntTpe     || tpe =:= c.typeOf[java.lang.Integer  ]


        //||
        //||
      ) {
        onameWithSelector(q"t.asNumber().intValue()")
      } else if (tpe =:= LongTpe    || tpe =:= c.typeOf[java.lang.Long     ]) {
        onameWithSelector(q"t.asNumber().longValue()")
      } else if (tpe =:= FloatTpe    || tpe =:= c.typeOf[java.lang.Float     ]) {
        onameWithSelector(q"t.asNumber().doubleValue().toFloat")
      } else if (tpe =:= DoubleTpe  || tpe =:= c.typeOf[java.lang.Double   ]) {
        onameWithSelector(q"t.asNumber().doubleValue()")
      } else if (tpe =:= BooleanTpe || tpe =:= c.typeOf[java.lang.Boolean  ]) {
        onameWithSelector(q"t.asBoolean().getValue()")
      } else if(tpe =:= CharTpe    || tpe =:= c.typeOf[java.lang.Character] || tpe =:= c.typeOf[java.lang.String] ) {
        onameWithSelector(q"t.asString().getValue()")
      } else if (tpe.typeSymbol.fullName == "org.bson.types.ObjectId" ) {
        onameWithSelector(q"t.asObjectId().getValue()")
      } else if (tpe.typeSymbol.fullName == "scala.Option") {
        val tpeElement = tpe.typeArgs.head
        val unpackFunName = TermName(c.freshName("unpack$"))
        val unpackFun = q"def $unpackFunName(x:BsonValue) = { ${unpackOne("x","",tpeElement,None)} }"
        q"""{
            $unpackFun
           val t=${nameAsTree(term)}.get($field)
           if  ( t == null || t.isNull() ) {
              None
           } else {
             Some($unpackFunName(t))
           }
        }"""
      } else if (tpe.typeSymbol.fullName == "scala.Tuple2") {
        val tp1 = tpe.typeArgs.head
        val tp2 = tpe.typeArgs.tail.head

        val unpackFunName = TermName(c.freshName("unpack$"))
        val unpackFun = q"def $unpackFunName(x:BsonValue) = { ${unpackOne("x","",tp1,None)} }"

        q"""
           val t=$oname
           if ( ! t.isArray() ) {
            throw new _root_.mondao.ConvertException($field,"is not Array but '" + t.getClass.toString + "'")
           }
           val ta = t.asArray()
           if (ta.size() < 2) {
            throw new _root_.mondao.ConvertException($field,"is Array but len must be equal 2")
           }

           ({
              val tplcnv1 = ta.get(0)
              ${unpackOne("tplcnv1", "", tp1, None)}
           }
            ,
           {
              val tplcnv2 = ta.get(1)
              ${unpackOne("tplcnv2", "", tp2, None)}
            }
           )

         """
      } else if (tpe.baseClasses.exists(_.fullName == "scala.collection.GenMapLike")) {
        val tpeKey: c.universe.Type = tpe.typeArgs.head
        val tpeElement = tpe.typeArgs.tail.head
        val unpackFunName = TermName(c.freshName("unpack$"))
        val unpackFun = q"def $unpackFunName(x:BsonValue) = { ${unpackOne("x","",tpeElement,None)} }"
        if (tpeKey =:= c.typeOf[String]) {
          //BsonDocument().keySet().fo
          q"""{
            $unpackFun
           val t=${nameAsTree(term)}.get($field)
           if  ( t == null ) {
              ${tpe.typeSymbol.companion}.empty[..${tpe.typeArgs}]
           } else if ( ! t.isDocument() ) {
             throw new _root_.mondao.ConvertException($field,"is not Document but '" + t.getClass.toString + "'")
           } else {
            val bldr = ${tpe.typeSymbol.companion}.newBuilder[..${tpe.typeArgs}]
            val ta = t.asDocument()
            val it = ta.keySet().iterator()
            while ( it.hasNext() ) {
              val k = it.next()
              val o = ta.get(k)
              bldr += k -> $unpackFunName(o)
            }
            bldr.result
           }
        }"""
        } else if ( tpeKey.baseClasses.exists(_.fullName == "scala.Enumeration.Value") ){
            val baseEnumClass = nameAsTree(tpeKey.toString().split('.').dropRight(1).mkString("."))
            q"""{
              $unpackFun
             val t=${nameAsTree(term)}.get($field)
             if  ( t == null ) {
                ${tpe.typeSymbol.companion}.empty[..${tpe.typeArgs}]
             } else if ( ! t.isDocument() ) {
               throw new _root_.mondao.ConvertException($field,"is not Document but '" + t.getClass.toString + "'")
             } else {
              val bldr = ${tpe.typeSymbol.companion}.newBuilder[..${tpe.typeArgs}]
              val ta = t.asDocument()
              val it = ta.keySet().iterator()
              while ( it.hasNext() ) {
                val k = it.next()
                val o = ta.get(k)
                bldr += $baseEnumClass.withName(k) -> $unpackFunName(o)
              }
              bldr.result
             }
          }"""
        } else {
          c.abort(c.enclosingPosition, s"Key for dict is not to string convertable $tpeKey")
        }
      } else if (tpe.typeSymbol.fullName == "scala.Array" || tpe.baseClasses.exists(_.fullName == "scala.collection.Traversable")) { // Array is final class
        val tpeElement = tpe.typeArgs.head
        val unpackFunName = TermName(c.freshName("unpack$"))
        val unpackFun = q"def $unpackFunName(x:BsonValue) = { ${unpackOne("x","",tpeElement,None)} }"
        q"""{
            $unpackFun
           val t=${nameAsTree(term)}.get($field)
           if  ( t == null ) {
              ${tpe.typeSymbol.companion}.empty[..${tpe.typeArgs}]
           } else if ( ! t.isArray() ) {
             throw new _root_.mondao.ConvertException($field,"is not array")
           } else {
            val bldr = ${tpe.typeSymbol.companion}.newBuilder[..${tpe.typeArgs}]
            val ta = t.asArray()
            val sz = ta.size()
            var i = 0
            while ( i < sz ) {
              val o = ta.get(i)
              bldr += $unpackFunName(o)
              i+=1
            }
            bldr.result
           }
        }"""
    //BsonArray().get
        /*val tmp = TermName(c.freshName("x$"))
        val innerType = tpe.typeArgs.head
        val packAst = packOne("x",innerType)

        val packFun = q"def $tmp(x:$innerType) = { $packAst }"
        q"BsonArray({ $packFun ; $oname.map($tmp) })"
        */
      } else if ( tpe.baseClasses.exists(_.fullName == "scala.Enumeration.Value") ){
        val baseEnumClass = nameAsTree(tpe.toString().split('.').dropRight(1).mkString("."))
        q"""$baseEnumClass.withName($oname.asString().getValue())"""
      } else {
        //c.abort(c.enclosingPosition, s"unpackOne - type $tpe can not be converted to Bson")
        q"""_root_.mondao.Convert.fromBson[$tpe]($oname).get"""
      }
    }

    val tpe: c.universe.Type = weakTypeOf[A]
    val companion: c.universe.Symbol = tpe.typeSymbol.companion

    if (companion == NoSymbol ) {
      c.abort(c.enclosingPosition, s"Companion symbol not found. Can't find companien for inner classes")
    }


    val constructorSym = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get

    val fields = constructorSym.paramLists.head

    //val applyMethod = companion.typeSignature.decl(TermName("apply")).asMethod
    //val constructorMethod = constructorSym

    val defaults = fields.map(_.asTerm).zipWithIndex.map { case (p,i)  =>
      if (! p.isParamWithDefault) None
      else {
        val getterName = TermName("apply$default$" + (i + 1))
        Some(q"$companion.$getterName")
      }
    }

    //println("constructorSym",constructorSym.asMethod,companion.typeSignature.decl(TermName("apply")).asMethod)

    val fromDBObject: List[c.universe.Tree] = (fields zip defaults) .map { case (field,default) ⇒
      val name: c.universe.TermName = field.name.toTermName
      unpackOne("o" , name.decodedName.toString,field.typeSignature,default)
    }

    //val fromDBObject: List[c.universe.Tree] = List()
    val ret = c.Expr[Reads[A]] {
      q"""
         new _root_.mondao.Reads[$tpe] {
            import _root_.org.bson.types.ObjectId
            import _root_.org.mongodb.scala.bson._
            def reads(_o:BsonValue) = try {
              if ( ! _o.isInstanceOf[BsonDocument] ) throw new _root_.mondao.ConvertException("init","case class is not BsonDocument")
              val o = _o.asInstanceOf[BsonDocument]
              _root_.scala.util.Success($companion.apply(..$fromDBObject))
            } catch {
              case ex:Throwable => _root_.scala.util.Failure(ex)
            }
      }
    """
    }
    if (dbg) {
      println("Replaced macro for Reads: '" + weakTypeOf[A].toString+ "'")
      println(ret)
      println("--------------------------")
    }
    ret
  }

  def writesImpl[A: c.WeakTypeTag](c: Context) = _writesImpl[A](c,false)

  def writesImplDebug[A: c.WeakTypeTag](c: Context) = _writesImpl[A](c,true)


  def _writesImpl[A: c.WeakTypeTag](c: Context,dbg: Boolean): c.Expr[Writes[A]] = {
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
      } else if(tpe =:= BooleanTpe || tpe =:= c.typeOf[java.lang.Boolean  ]) {
        q"BsonBoolean($oname)"
      } else if(tpe =:= ByteTpe    || tpe =:= c.typeOf[java.lang.Byte     ]
        || tpe =:= ShortTpe   || tpe =:= c.typeOf[java.lang.Short    ]
        || tpe =:= IntTpe     || tpe =:= c.typeOf[java.lang.Integer  ]
        || tpe =:= LongTpe    || tpe =:= c.typeOf[java.lang.Long     ]
        || tpe =:= FloatTpe   || tpe =:= c.typeOf[java.lang.Float    ]
        || tpe =:= DoubleTpe  || tpe =:= c.typeOf[java.lang.Double   ]
        ) {
        q"BsonNumber($oname)"
      } else if (tpe.baseClasses.exists(_.fullName == "scala.Tuple2")) {
        val tp1 = tpe.typeArgs.head
        val tp2 = tpe.typeArgs.tail.head

        val packAst1 = packOne("xtpl2._1",tp1)
        val packAst2 = packOne("xtpl2._2",tp2)

        q"""{
            val a1 = { val xtpl2 = $oname; $packAst1 }
            val a2 = { val xtpl2 = $oname; $packAst2 }
           BsonArray(a1,a2)
        }"""
      } else if (tpe.baseClasses.exists(_.fullName == "scala.collection.GenMapLike")) {
        val tmp = TermName(c.freshName("x$"))
        val innerType = tpe.typeArgs.tail.head
        val keyType = tpe.typeArgs.head
        val packAst = packOne("x._2",innerType)
        val packFun = q"def $tmp(x:($keyType,$innerType)) = { (x._1.toString , $packAst) }"

        q"BsonDocument({ $packFun ; $oname.map($tmp) })"
      } else if ( tpe.baseClasses.exists(_.fullName == "scala.Enumeration.Value") ){
        q"BsonString($oname.toString)"
      } else if (tpe.typeSymbol.fullName == "scala.Option") {
        val tmp = TermName(c.freshName("x$"))
        val innerType = tpe.typeArgs.head
        val packAst = packOne("x",innerType)
        val packFun = q"def $tmp(x:$innerType) = { $packAst }"

        q"""{
            $packFun
            if ($oname.isDefined) $tmp($oname.get) else BsonNull()

        }"""
      } else if (tpe.typeSymbol.fullName == "org.bson.types.ObjectId" ) {
        q"BsonObjectId($oname)"
      } else if (tpe.typeSymbol.fullName == "scala.Array" || tpe.baseClasses.exists(_.fullName == "scala.collection.Traversable")) { // Array is final class
        val tmp = TermName(c.freshName("x$"))
        val innerType = tpe.typeArgs.head
        val packAst = packOne("x",innerType)

        val packFun = q"def $tmp(x:$innerType) = { $packAst }"
        q"BsonArray({ $packFun ; $oname.map($tmp) })"
      } else {
        q"_root_.mondao.Convert.toBson($oname)"
      }

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
         new _root_.mondao.Writes[$tpe] {
            import _root_.org.bson.types.ObjectId
            import _root_.org.mongodb.scala.bson._
            def writes(o:$tpe) = BsonDocument(
               ..$toDBObject
            )
      }
    """
    }

    if (dbg) {
      println("Replaced macro for Writes: '" + weakTypeOf[A].toString+ "'")
      println(ret)
      println("--------------------------")
    }

    ret
  }

}