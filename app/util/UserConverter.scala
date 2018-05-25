package util

import models.client.{GNMMadgexUser, MadgexUser, User, UserUpdateRequest}

object UserConverter{

  implicit def toGNMMadgexUser(user: User): GNMMadgexUser =
    GNMMadgexUser(
      user.id,
      MadgexUser(user.email, user.personalDetails.firstName, user.personalDetails.lastName, false, false)
    )

  implicit def toMadgexUser(user: User): MadgexUser =
    MadgexUser(user.email, user.personalDetails.firstName, user.personalDetails.lastName, false, false)

  implicit def toMadgexUser(user: UserUpdateRequest): MadgexUser =
    MadgexUser(user.email, user.firstName, user.lastName, false, false)

}
