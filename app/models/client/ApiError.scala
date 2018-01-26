package models.client

import play.api.http.Writeable
import play.api.libs.json._

import scala.language.implicitConversions

case class ApiError(message: String, details: String = "")

object ApiError {
  implicit val format = Json.format[ApiError]
}

