package models.database.mongo

import play.api.libs.functional.syntax._
import play.api.libs.json._

case class DeletedUser(
  id: String,
  email: String,
  username: String)

object DeletedUser {
  val deletedUserReads: Reads[DeletedUser] =  (
    (JsPath \ "_id").read[String] and
    (JsPath \ "email").read[String] and
    (JsPath \ "username").read[String]
  )(DeletedUser.apply _)

  val deletedUserWrites: OWrites[DeletedUser] = (
    (JsPath \ "_id").write[String] and
    (JsPath \ "email").write[String] and
    (JsPath \ "username").write[String]
  )(unlift(DeletedUser.unapply))

  implicit val format: OFormat[DeletedUser] =
    OFormat(deletedUserReads, deletedUserWrites)
}
