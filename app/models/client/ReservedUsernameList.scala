package models.client

import play.api.libs.json.Json

case class ReservedUsername(username: String)

object ReservedUsername {
  implicit val format = Json.format[ReservedUsername]
}

case class ReservedUsernameList(reservedUsernames: List[String] = Nil)

object ReservedUsernameList {
  implicit val format = Json.format[ReservedUsernameList]
}
