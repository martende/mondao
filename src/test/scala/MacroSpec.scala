import mondao.{Reads, Writes}
import org.bson.types.ObjectId
import org.mongodb.scala.bson._
import org.scalatest.FunSuite

import scala.util.{Failure, Success, Try}

class WritesSpec extends FunSuite {
  test("test1") {
    case class AAA(a:Int,b:String)
    implicit val w1 = new Writes[AAA] {
      def writes(o:AAA) = BsonDocument(
        "a" -> BsonNumber(o.a),
        "b" -> BsonString(o.b)
      )
    }

    implicit val r1 = new Reads[AAA] {
      def reads(_o:BsonValue):Try[AAA] = try {
        val o = _o.asDocument()
        Success(AAA(o.getNumber("a").intValue,o.getString("b").getValue))
      } catch {
        case ex:Throwable => Failure(ex)
      }
    }

    val d = mondao.Convert.toBson(AAA(10,"20"))
    assert(d == BsonDocument("a" -> 10, "b" -> "20"))

    assert(AAA(10,"20") == mondao.Convert.fromBson(d).get)
  }
  test("test2") {
    case class AAA2(a:Int,b:String)
    implicit val w1 = mondao.Macros.writes[AAA2]
    val d = mondao.Convert.toBson(AAA2(10,"20"))
    assert(d == BsonDocument("a" -> 10, "b" -> "20"))
  }

  test("test3") {
    case class BBB(a:Int)
    case class AAA(a:Int,b:BBB)
    implicit val w1 = mondao.Macros.writes[BBB]
    implicit val w2 = mondao.Macros.writes[AAA]
    val d = mondao.Convert.toBson(AAA(10,BBB(20)))
    assert(d == BsonDocument("a" -> 10, "b" -> BsonDocument("a" -> 20)))
  }


  test("test4") {
    case class AAA(a:Array[Int])
    implicit val w2 = mondao.Macros.writes[AAA]
    val d = mondao.Convert.toBson(AAA(Array(10,20,30)))
    assert(d == BsonDocument("a" -> BsonArray(10,20,30)))
  }

  test("test5") {
    case class AAA(a:List[Int])
    implicit val w2 = mondao.Macros.writes[AAA]
    val d = mondao.Convert.toBson(AAA(List(10,20,30)))
    assert(d == BsonDocument("a" -> BsonArray(10,20,30)))
  }


  test("test6") {
    case class AAA(a:Option[Int])
    implicit val w2 = mondao.Macros.writes[AAA]
    val d = mondao.Convert.toBson(AAA(Some(10)))
    assert(d == BsonDocument("a" -> 10))
    val d2 = mondao.Convert.toBson(AAA(None))
    assert(d2 == BsonDocument("a" -> BsonNull()))
  }

  test("test7") {
    case class AAA(a:Map[String,Int])
    implicit val w2 = mondao.Macros.writes[AAA]
    val d = mondao.Convert.toBson(AAA(Map("a"->10)))
    assert(d == BsonDocument("a" -> BsonDocument("a" ->10)))
  }

  test("test8") {
    object ImageTagsTp extends Enumeration {
      val faceLeft = Value
      val faceRight = Value
      val faceAn = Value
    }
      case class AAA(a:Map[ImageTagsTp.Value,Boolean])
    implicit val w2 = mondao.Macros.writes[AAA]
    val d = mondao.Convert.toBson(AAA(Map(ImageTagsTp.faceLeft->true)))
    assert(d == BsonDocument("a" -> BsonDocument("faceLeft" ->true)))
  }


  test("test9") {
    case class AAA(a:String,b:ObjectId)
    implicit val w2 = mondao.Macros.writes[AAA]
    val oid = new ObjectId()
    val d = mondao.Convert.toBson(AAA("10",oid))
    assert(d == BsonDocument("a" -> "10", "b" -> oid))
  }


  test("test10") {
    object ImageTagsTp extends Enumeration {
      val faceLeft = Value
      val faceRight = Value
      val faceAn = Value
    }
    case class AAA(a:ImageTagsTp.Value)
    implicit val w2 = mondao.Macros.writes[AAA]

    val d = mondao.Convert.toBson(AAA(ImageTagsTp.faceLeft))
    assert(d == BsonDocument("a" -> "faceLeft"))
  }


  test("test11") {
    case class AAA(a:(String,Int))
    implicit val w2 = mondao.Macros.writes[AAA]
    val d = mondao.Convert.toBson(AAA(("10",20)))
    assert(d == BsonDocument("a" -> BsonArray("10",20) ))
  }

  test("test12") {
    case class AAA(a:Option[(String,Int)])
    implicit val w2 = mondao.Macros.writes[AAA]
    val d = mondao.Convert.toBson(AAA(Some(("10",20))))
    assert(d == BsonDocument("a" -> BsonArray("10",20) ))
  }


}

class ReadSpec extends FunSuite {

  case class ctest1(a:Int)
  test("test1") {
    implicit val w1 = mondao.Macros.reads[ctest1]
    val d = BsonDocument("a" -> 12)
    val a = mondao.Convert.fromBson(d).get
    assert(a == ctest1(12))
  }

  case class ctest2(a:Long)
  test("test2") {
    implicit val w1 = mondao.Macros.reads[ctest2]
    val d = BsonDocument("a" -> 12)
    val a = mondao.Convert.fromBson(d).get
    assert(a == ctest2(12))
  }

  case class ctest3(a:Double)
  test("test3") {
    implicit val w1 = mondao.Macros.reads[ctest3]
    val d = BsonDocument("a" -> 12.0)
    val a = mondao.Convert.fromBson(d).get
    assert(a == ctest3(12))
  }

  case class ctest40(a:Float)
  case class ctest41(b:Float)
  test("test4") {
    implicit val w1 = mondao.Macros.reads[ctest40]
    implicit val w2 = mondao.Macros.reads[ctest41]
    val d = BsonDocument("a" -> 12.0)
    val a = mondao.Convert.fromBson[ctest40](d).get
    assert(a == ctest40(12.0f))
  }



  case class ctest50(a:Int)
  case class ctest5(a:Int,b:ctest50)
  test("test5") {
    implicit val w2 = mondao.Macros.reads[ctest50]

    implicit val w1 = mondao.Macros.reads[ctest5]

    val d = BsonDocument("a" -> 11,"b" -> BsonDocument("a" -> 12))
    val a = mondao.Convert.fromBson[ctest5](d).get
    assert(a == ctest5(11,ctest50(12)))

    val d2 = BsonDocument("a" -> 11,"b" -> BsonDocument("a0" -> 12))
    val a2 = mondao.Convert.fromBson[ctest5](d2)
    assert(a2.isFailure)
  }


  case class ctest6(a:Boolean)
  test("test6") {
    implicit val w1 = mondao.Macros.reads[ctest6]
    val d = BsonDocument("a" -> true)
    val a = mondao.Convert.fromBson[ctest6](d).get
    assert(a == ctest6(true))
  }

  case class ctest7(a:String)
  test("test7") {
    implicit val w1 = mondao.Macros.reads[ctest7]
    val d = BsonDocument("a" -> "A")
    val a = mondao.Convert.fromBson[ctest7](d).get
    assert(a == ctest7("A"))
  }


  case class ctest8(a:Array[String])
  test("test8") {
    implicit val w1 = mondao.Macros.reads[ctest8]
    val d = BsonDocument("a" -> BsonArray("A","B"))
    val a = mondao.Convert.fromBson[ctest8](d).get
    assert(a.a.toList == List("A","B"))
  }

  case class ctest9(a:Array[Int])
  test("test9") {
    implicit val w1 = mondao.Macros.reads[ctest9]
    val d = BsonDocument("a" -> BsonArray(1,2))
    val a = mondao.Convert.fromBson[ctest9](d).get
    assert(a.a.toList == List(1,2))
  }


  case class ctest10(a:List[Int])
  test("test10") {
    implicit val w1 = mondao.Macros.reads[ctest10]
    val d = BsonDocument("a" -> BsonArray(1,2))
    val a = mondao.Convert.fromBson[ctest10](d).get
    assert(a.a == List(1,2))
  }


  case class ctest11(a:Map[String,Int])
  test("test11") {
    implicit val w1 = mondao.Macros.reads[ctest11]
    val d = BsonDocument("a" -> BsonDocument("a" -> 1))
    val a = mondao.Convert.fromBson[ctest11](d).get
    assert(a == ctest11(Map("a"->1)))
  }


  object ImageTagsTp extends Enumeration {
    val faceLeft = Value
    val faceRight = Value
    val faceAn = Value
  }

  case class ctest12(a:Map[ImageTagsTp.Value,Int])
  test("test12") {
    implicit val w1 = mondao.Macros.reads[ctest12]
    val d = BsonDocument("a" -> BsonDocument("faceLeft" -> 1,"faceRight" -> 2))
    val a = mondao.Convert.fromBson[ctest12](d).get
    assert(a == ctest12(Map(ImageTagsTp.faceLeft->1,ImageTagsTp.faceRight->2)))
  }


  case class ctest13(a:String,b:Option[ObjectId])

  test("test13") {
    implicit val w2 = mondao.Macros.reads[ctest13]
    val oid = new ObjectId()
    val d = BsonDocument("a" -> "A","b" -> BsonObjectId(oid))
    val a = mondao.Convert.fromBson[ctest13](d).get
    assert(a == ctest13("A",Some(oid)))
    val d2 = BsonDocument("a" -> "B","b" -> BsonNull())
    val a2 = mondao.Convert.fromBson[ctest13](d2).get
    assert(a2 == ctest13("B",None))
  }

  object test14Enum extends Enumeration {
    val faceLeft = Value
    val faceRight = Value
    val faceAn = Value
  }

  case class ctest14(a:test14Enum.Value)

  test("test14") {
    implicit val w2 = mondao.Macros.reads[ctest14]
    val d = BsonDocument("a" -> "faceLeft")
    val a = mondao.Convert.fromBson[ctest14](d).get
    assert(a == ctest14(test14Enum.faceLeft))
  }

  case class ctest15(a:Boolean=true,b:Int)

  test("test15") {
    implicit val w2 = mondao.Macros.reads[ctest15]
    val d = BsonDocument("b"->10)
    val a = mondao.Convert.fromBson[ctest15](d).get
    assert(a == ctest15(true,10))
  }

  case class ctest16(a:(Boolean,Int))

  test("test16") {
    implicit val w2 = mondao.Macros.reads[ctest16]
    val d = BsonDocument("a"->BsonArray(true,10))
    val a = mondao.Convert.fromBson[ctest16](d).get
    assert(a == ctest16((true,10)))
  }


  case class ctest17(a:Option[(Boolean,Int)])

  test("test17") {
    implicit val w2 = mondao.Macros.reads[ctest17]
    val d = BsonDocument("a"->BsonArray(true,10))
    val a = mondao.Convert.fromBson[ctest17](d).get
    assert(a == ctest17(Some((true,10))))
  }


  case class ctest18(a:(Option[Int], Option[Int]))

  test("class ctest18(a:(Option[Int], Option[Int]))") {
    implicit val w2 = mondao.Macros.reads[ctest18]
    val d = BsonDocument("a"->BsonArray(10,20))
    val a = mondao.Convert.fromBson[ctest18](d).get
    assert(a == ctest18((Some(10),Some(20))))
  }

  test("class ctest18(a:(Option[Int], Option[Int])) on BsonArray(10, BsonNull())") {
    implicit val w2 = mondao.Macros.readsDebug[ctest18]
    val d2 = BsonDocument("a" -> BsonArray(10, BsonNull()))
    val a2 = mondao.Convert.fromBson[ctest18](d2).get
    assert(a2 == ctest18((Some(10), None)))
  }

}
