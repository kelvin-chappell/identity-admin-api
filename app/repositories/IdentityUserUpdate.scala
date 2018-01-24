package repositories

import models.UserUpdateRequest

case class IdentityUserUpdate(
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
  userEmailValidated: Option[Boolean] = None)

object IdentityUserUpdate {
  def apply(userUpdateRequest: UserUpdateRequest, userEmailValidated: Option[Boolean]): IdentityUserUpdate =
    IdentityUserUpdate(
      email = userUpdateRequest.email,
      username = userUpdateRequest.username,
      displayName = userUpdateRequest.displayName,
      firstName = userUpdateRequest.firstName,
      lastName = userUpdateRequest.lastName,
      location = userUpdateRequest.location,
      aboutMe = userUpdateRequest.aboutMe,
      interests = userUpdateRequest.interests,
      receiveGnmMarketing = userUpdateRequest.receiveGnmMarketing,
      receive3rdPartyMarketing = userUpdateRequest.receive3rdPartyMarketing,
      userEmailValidated = userEmailValidated
    )
}
