package util

import models.client.{GNMMadgexUser, MadgexUser, User, UserUpdateRequest}

object UserConverter{

  implicit def toGNMMadgexUser(user: User): GNMMadgexUser = {
    GNMMadgexUser(user.id, MadgexUser(user.email, user.personalDetails.firstName, user.personalDetails.lastName,
      user.status.receive3rdPartyMarketing.getOrElse(false), user.status.receiveGnmMarketing.getOrElse(false))
    )
  }

  implicit def toMadgexUser(user: User): MadgexUser =
    MadgexUser(user.email, user.personalDetails.firstName, user.personalDetails.lastName,
      user.status.receive3rdPartyMarketing.getOrElse(false), user.status.receiveGnmMarketing.getOrElse(false))

  implicit def toMadgexUser(user: UserUpdateRequest): MadgexUser =
    MadgexUser(user.email, user.firstName, user.lastName,
      user.receive3rdPartyMarketing.getOrElse(false), user.receiveGnmMarketing.getOrElse(false))

}
