package models

import org.joda.time.{DateTime, LocalDate}
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
}
