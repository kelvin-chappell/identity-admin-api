package models.client

import ClientJsonFormats._
import models.database.mongo.{IdentityUser, Orphan, PersistedUser}
import org.joda.time.DateTime
import play.api.libs.json.Json
import scala.language.implicitConversions

case class UserSummary(id: String,
                       email: String,
                       username: Option[String] = None,
                       firstName: Option[String] = None,
                       lastName: Option[String] = None,
                       creationDate: Option[DateTime] = None,
                       lastActivityDate: Option[DateTime] = None,
                       registrationIp: Option[String] = None,
                       lastActiveIpAddress: Option[String] = None,
                       orphan: Boolean)

object UserSummary {
  implicit val format = Json.format[UserSummary]

  def fromPersistedUser(persistedUser: PersistedUser): UserSummary =
    persistedUser match {
      case user: IdentityUser =>
        UserSummary(
          id = user._id,
          email = user.primaryEmailAddress,
          username = user.publicFields.flatMap(_.username),
          firstName = user.privateFields.flatMap(_.firstName),
          lastName = user.privateFields.flatMap(_.secondName),
          creationDate = user.dates.flatMap(_.accountCreatedDate),
          lastActivityDate = user.dates.flatMap(_.lastActivityDate),
          registrationIp = user.privateFields.flatMap(_.registrationIp),
          lastActiveIpAddress= user.privateFields.flatMap(_.lastActiveIpAddress),
          orphan = false
        )

      case user: Orphan => UserSummary(id = user.id, email = user.email, orphan = true)
  }

}
