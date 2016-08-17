package mondao

import org.joda.time.DateTimeZone
import org.mongodb.scala.bson._

import scala.util.{Failure, Success, Try}


object Implicits {
  implicit val jodaDateTimeW = new Writes[org.joda.time.DateTime] {
    def writes(o:org.joda.time.DateTime) = BsonDateTime(o.getMillis)
  }

  implicit val jodaDateTimeR = new Reads[org.joda.time.DateTime] {
    def reads(_o:BsonValue):Try[org.joda.time.DateTime] = try {
      Success(new org.joda.time.DateTime(_o.asDateTime().getValue()).withZone(DateTimeZone.UTC))
    } catch {
      case ex:Throwable => Failure(ex)
    }
  }
}
