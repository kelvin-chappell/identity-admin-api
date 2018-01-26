package models.database.mongo

import org.joda.time.{DateTime, DateTimeZone}
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._

// Used by ReactiveMongo for implicit JSON to BSON conversion
object MongoJsonFormats {

  // https://stackoverflow.com/questions/41390385/how-to-store-date-in-mongodb-in-iso-format-instead-of-long-play-scala-and-reac
  implicit val dateTimeRead: Reads[DateTime] =
    (__ \ "$date").read[Long].map(dateTime => new DateTime(dateTime, DateTimeZone.UTC))

  implicit val dateTimeWrite: Writes[DateTime] = new Writes[DateTime] {
    def writes(dateTime: DateTime): JsValue = Json.obj("$date" -> dateTime.getMillis)
  }

  implicit val objectMapFormat = new Format[Map[String, Any]] {

    def writes(map: Map[String, Any]): JsValue =
      Json.obj(map.map{case (s, o) =>
        val ret:(String, JsValueWrapper) = o match {
          case _:String => s -> JsString(o.asInstanceOf[String])
          case _:Int => s -> JsNumber(s.asInstanceOf[Int])
          case _:Long => s -> JsNumber(s.asInstanceOf[Long])
          case _:Double => s -> JsNumber(s.asInstanceOf[Double])
          case _:Boolean => s -> JsBoolean(s.asInstanceOf[Boolean])
          case other => s -> JsString(other.toString)
        }
        ret
      }.toSeq:_*)


    def reads(jv: JsValue): JsResult[Map[String, Any]] =
      JsSuccess(jv.as[Map[String, JsValue]].map{case (k, v) =>
        k -> (v match {
          case s:JsString => s.as[String]
          case i:JsNumber => i.as[Long]
          case b:JsBoolean => b.as[Boolean]
          case other => other.toString()
        })
      })
  }
}
