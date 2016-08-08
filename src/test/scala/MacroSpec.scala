import mondao.{Reads, Writes}
import org.mongodb.scala.bson.{BsonDocument, _}
import org.scalatest.FunSuite

import scala.util.{Failure, Success, Try}

class MacroSpec extends FunSuite {
  /*
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
  }*/

  test("test4") {
    case class BBB(a:Int)
    case class AAA(a:Int,b:BBB)
    implicit val w1 = new Writes[BBB] {
      def writes(o:BBB) = BsonDocument(
        "a" -> BsonNumber(o.a)
      )
    }
    implicit val w2 = mondao.Macros.writes[AAA]
    val d = mondao.Convert.toBson(AAA(10,BBB(20)))
    assert(d == BsonDocument("a" -> 10, "b" -> BsonDocument("a" -> 20)))
  }
}
