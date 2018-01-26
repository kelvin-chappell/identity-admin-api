package models.client

import play.api.libs.json.Json

case class UserUpdateRequest(
  email: String,
  username: Option[String] = None,
  displayName: Option[String] = None,
  firstName: Option[String] = None,
  lastName: Option[String] = None,
  location: Option[String] = None,
  aboutMe: Option[String] = None,
  interests: Option[String] = None,
  receiveGnmMarketing: Option[Boolean] = None,
  receive3rdPartyMarketing: Option[Boolean] = None,
  consents: List[Consent] = Nil,
  userEmailValidated: Option[Boolean] = None)

object UserUpdateRequest {
  implicit val format = Json.format[UserUpdateRequest]
}
