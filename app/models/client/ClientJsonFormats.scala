package models.client

import org.joda.time.{DateTime, LocalDate}
import play.api.http.Writeable
import play.api.libs.json._

object ClientJsonFormats { // Format for identity-admin client of the API
  // DateTime format
  private val dateTimePattern = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"
  implicit val dateReads = JodaReads.jodaDateReads(dateTimePattern)
  implicit val dateWrite = JodaWrites.jodaDateWrites(dateTimePattern)
  implicit val dateTimeFormat = Format[DateTime](dateReads, dateWrite)

  // LocalDate format
  implicit val localDateFormat =
    Format[LocalDate](JodaReads.DefaultJodaLocalDateReads, JodaWrites.DefaultJodaLocalDateWrites)

  /* Enables us to write, BadRequest(ApiError("some message")), vs. BadRequest(Json.toJson(ApiError("some message")))
   https://groups.google.com/forum/#!topic/play-framework/o93EEdg-fUA */
  implicit def jsWriteable[A](implicit wa: Writes[A], wjs: Writeable[JsValue]): Writeable[A] =
    wjs.map(a => Json.toJson(a))
}
