package controllers

import models.client.ClientJsonFormats._
import javax.inject.{Inject, Singleton}

import actions.{AuthenticatedAction, IdentityUserAction, OrphanUserAction}
import com.gu.identity.util.Logging
import com.gu.tip.Tip
import configuration.Config
import play.api.libs.json._
import play.api.mvc._
import services._

import scala.concurrent.{ExecutionContext, Future}
import scalaz._
import scalaz.std.scalaFuture._
import scalaz.std.string._
import scalaz.syntax.validation._
import scalaz.syntax.apply._
import scalaz.syntax.std.boolean._
import models.client.{ApiError, UserUpdateRequest, UserUpdateRequestValidator}
import models.client.ApiError._

@Singleton class UsersController @Inject() (
    cc: ControllerComponents,
    userService: UserService,
    auth: AuthenticatedAction,
    identityUserAction: IdentityUserAction,
    orphanUserAction: OrphanUserAction,
    salesforce: SalesforceService,
    discussionService: DiscussionService,
    exactTargetService: ExactTargetService)(implicit ec: ExecutionContext) extends AbstractController(cc) with Logging {

  def search(query: String, limit: Option[Int], offset: Option[Int]) = auth.async { request =>
    import Config.SearchValidation._

    val queryValid =
      (query.length < minimumQueryLength) ? s"query must be a minimum of $minimumQueryLength characters".failure[String] | query.success[String]

    val limitValid =
      limit.exists(_ < 0) ? "limit must be a positive integer".failure[Option[Int]] | limit.success[String]

    val offsetValid =
      offset.exists(_ < 0) ? "offset must be a positive integer".failure[Option[Int]] | offset.success[String]

    (queryValid |@| limitValid |@| offsetValid) { (query, limit, offset) =>
      EitherT(userService.search(query, limit, offset)).fold(
        error => InternalServerError(error),
        response => Ok(Json.toJson(response))
      )
    } match {
      case Success(result) => result
      case Failure(error) => Future.successful(BadRequest(ApiError(error)))
    }
  }

  def unreserveEmail(id: String) = auth.async { request =>
    userService.unreserveEmail(id).map(_ => NoContent)
  }

  def findById(id: String) = (auth andThen identityUserAction(id)) { request =>
    Ok(request.user)
  }

  def findOrphanByEmail(email: String) = (auth andThen orphanUserAction(email)) { request =>
    Ok(request.user)
  }

  def update(id: String) = (auth andThen identityUserAction(id)).async(parse.json) { request =>
    request.body.validate[UserUpdateRequest] match {
      case JsSuccess(result, path) =>
        UserUpdateRequestValidator.isValid(result).fold(
          error => Future(BadRequest(ApiError("Failed to update user", error.message))),
          validUserUpdateRequest => {
            EitherT(userService.update(request.user.idapiUser, validUserUpdateRequest)).fold(
              error => InternalServerError(error),
              user => {
                if (Config.stage == "PROD") Tip.verify("User Update")
                Ok(Json.toJson(user))
              }
            )
          }
        )

      case JsError(e) => Future(BadRequest(ApiError("Failed to update user", e.toString)))
    }
  }

  def delete(id: String) = (auth andThen identityUserAction(id)).async { request =>
    logger.info(s"Deleting user $id")

    val deleteEmailSubscriberF = EitherT(exactTargetService.deleteSubscriber(request.user.idapiUser.email))
    val deleteAccountF = EitherT(userService.delete(request.user))

    (for {
      _ <- deleteEmailSubscriberF
      _ <- deleteAccountF
    } yield {
      EmailService.sendDeletionConfirmation(request.user.idapiUser.email)
    }).fold(
      error => {
        logger.error(s"Error deleting user $id: $error")
        InternalServerError(error)
      },
      _ => {
        logger.info(s"Successfully deleted user $id")
        NoContent
      }
    )
  }

  def unsubcribeFromAllEmailLists(email: String) = auth.async { request =>
    logger.info("Unsubscribing from all email lists (marketing and editorial)")
    val unsubscribeMarketingEmailsInIdentity = EitherT(exactTargetService.unsubscribeFromAllLists(email))
    val unsubcribeAllEmailsInExactTarget = EitherT(userService.unsubscribeFromMarketingEmails(email))

    (for {
      _ <- unsubscribeMarketingEmailsInIdentity
      _ <- unsubcribeAllEmailsInExactTarget
    } yield ()).fold(
      error => {
        logger.error(s"Failed to unsubscribe from all email lists: $error")
        InternalServerError(error)
      },
      _ => {
        logger.info(s"Successfully unsubscribed from all email lists")
        NoContent
      }
    )
  }

  def activateEmailSubscriptions(email: String) = auth.async { request =>
    logger.info("Activate email address in ExactTarget")

    EitherT(exactTargetService.activateEmailSubscription(email)).fold(
      error => {
        logger.error(s"Failed to activate email subscriptions: $error")
        InternalServerError(error)
      },
      _ => {
        logger.info(s"Successfully activated email subscriptions")
        NoContent
      }
    )
  }

  def sendEmailValidation(id: String) = (auth andThen identityUserAction(id)).async { request =>
    logger.info(s"Sending email validation for user with id: $id")
    EitherT(userService.sendEmailValidation(request.user.idapiUser)).fold(
      error => InternalServerError(error),
      _ => NoContent
    )
  }

  def validateEmail(id: String) = (auth andThen identityUserAction(id)).async { request =>
    logger.info(s"Validating email for user with id: $id")
    EitherT(userService.validateEmail(request.user.idapiUser)).fold(
      error => InternalServerError(error),
      _ => NoContent
    )
  }
}
