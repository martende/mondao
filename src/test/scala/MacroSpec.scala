import mondao.{Reads, Writes}
import org.mongodb.scala.bson.{BsonDocument, _}
import org.scalatest.FunSuite

import scala.util.{Failure, Success, Try}
/*
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

}
*/
class ReadSpec extends FunSuite {
  test("test1") {
    case class AAA(a:Int)
    implicit val w1 = mondao.Macros.reads[AAA]
    val d = BsonDocument("a" -> 10)
    val a = mondao.Convert.fromBson(d)
    assert(a == AAA(10))
  }

}