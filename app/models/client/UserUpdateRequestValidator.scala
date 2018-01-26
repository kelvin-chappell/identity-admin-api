package models.client

import scalaz.{-\/, \/, \/-}

case class ValidationError(message: String)

object UserUpdateRequestValidator {

  def isValid(userUpdateRequest: UserUpdateRequest): ValidationError \/ UserUpdateRequest = {
    val validUsernameAndDisplayName = (userUpdateRequest.username, userUpdateRequest.displayName) match {
      case (Some(newUsername), Some(newDisplayName)) => newUsername == newDisplayName
      case _ => true
    }
    if (validUsernameAndDisplayName) {
      \/-(userUpdateRequest)
    } else {
      -\/(ValidationError("Conflict between username and display name"))
    }
  }

}
