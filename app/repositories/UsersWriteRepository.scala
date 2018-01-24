package repositories

import javax.inject.{Inject, Singleton}

import com.gu.identity.util.Logging
import models._
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.play.json.collection._
import reactivemongo.play.json._
import play.api.libs.json.Json

import scala.concurrent.ExecutionContext
import scalaz.{-\/, EitherT, OptionT, \/-}
import scalaz.std.scalaFuture._

@Singleton class UsersWriteRepository @Inject() (
    reactiveMongoApi: ReactiveMongoApi,
    deletedUsersRepository: DeletedUsersRepository)(implicit ec: ExecutionContext) extends Logging {

  private lazy val usersF = reactiveMongoApi.database.map(_.collection("users"))

  private def findBy(key: String): ApiResponse[IdentityUser] =
    OptionT(usersF.flatMap(_.find(selector(key)).one[IdentityUser])).fold(
      user => \/-(user),
      -\/(ApiError("User not found"))
    )

  private def selector(key: String) =
    Json.obj(
      "$or" -> Json.arr(
        Json.obj("_id" -> key.toLowerCase),
        Json.obj("primaryEmailAddress" -> key.toLowerCase)
      )
    )

  def update(user: User, userUpdateRequest: UserUpdateRequest): ApiResponse[User] =
    (for {
      persistedUser <- EitherT(findBy(user.id))
      updatedUser <- EitherT(doUpdate(persistedUser.mergeWith(userUpdateRequest)))
    } yield (updatedUser)).run

  def updateEmailValidationStatus(user: User, emailValidated: Boolean): ApiResponse[User] =
    (for {
      persistedUser <- EitherT(findBy(user.id))
      statusFields = persistedUser.statusFields.getOrElse(StatusFields()).copy(userEmailValidated = Some(emailValidated))
      updatedUser <- EitherT(doUpdate(persistedUser.copy(statusFields = Some(statusFields))))
    } yield (updatedUser)).run

  private def doUpdate(userToSave: IdentityUser): ApiResponse[User] =
    usersF
      .flatMap(_.update(selector(userToSave._id), userToSave))
      .map( _ => \/-(User.fromIdentityUser(userToSave)))
      .recover { case error =>
        val title = s"Failed to update user ${userToSave._id}"
        logger.error(title, error)
        -\/(ApiError(title, error.getMessage))
      }

  def delete(user: User): ApiResponse[Unit] =
    usersF
      .flatMap(_.remove(selector(user.id)))
      .map(_ => \/-{})
      .recover { case error =>
        val title = s"Failed to delete user ${user.id}"
        logger.error(title, error)
        -\/(ApiError(title, error.getMessage))
      }

  def unsubscribeFromMarketingEmails(email: String): ApiResponse[User] =
    (for {
      persistedUser <- EitherT(findBy(email))
      statusFields = persistedUser.statusFields.getOrElse(StatusFields()).copy(receive3rdPartyMarketing = Some(false), receiveGnmMarketing = Some(false))
      updatedUser <- EitherT(doUpdate(persistedUser.copy(statusFields = Some(statusFields))))
    } yield (updatedUser)).run
}
