package controllers

import java.io.PrintWriter
import models.client.ClientJsonFormats._
import javax.inject.{Inject, Singleton}
import actions.{AuthenticatedAction, IdentityUserAction, OrphanUserAction}
import actors.metrics.{MetricsActorProvider, MetricsSupport}
import akka.actor.ActorSystem
import com.typesafe.scalalogging.LazyLogging
import com.gu.tip.Tip
import configuration.Config
import play.api.data.Forms._
import play.api.data.Form
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
import models.client._
import models.client.ApiError._
import java.nio.file.Files

@Singleton class UsersController @Inject()(
    cc: ControllerComponents,
    userService: UserService,
    auth: AuthenticatedAction,
    identityUserAction: IdentityUserAction,
    orphanUserAction: OrphanUserAction,
    salesforce: SalesforceService,
    discussionService: DiscussionService,
    exactTargetService: ExactTargetService,
    override val metricsActorProvider: MetricsActorProvider)
    (implicit ec: ExecutionContext) extends AbstractController(cc) with LazyLogging with MetricsSupport {

  private implicit val metricsNamespace = MetricsSupport.Namespace("identity-admin")

  def search(query: String, limit: Option[Int], offset: Option[Int]) = auth.async { request =>
    import Config.SearchValidation._

    val queryValid =
      (query.length < minimumQueryLength) ? s"query must be a minimum of $minimumQueryLength characters".failure[String] | query.success[String]

    val limitValid =
      limit.exists(_ < 0) ? "limit must be a positive integer".failure[Option[Int]] | limit.success[String]

    val offsetValid =
      offset.exists(offset => offset < 0  && offset < 50) ? "offset must be a positive integer less than 50".failure[Option[Int]] | offset.success[String]

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

  def blockEmail(id: String) = auth.async { request =>
    userService.blockEmail(id) map {
      case \/-(_) => NoContent
      case -\/(error) => InternalServerError(error)
    }
  }

  def findById(id: String) = (auth andThen identityUserAction(id)) { request =>
    Ok(request.user)
  }

  def findIdentityUser(id: String) = auth.async { _ =>
    withMetricsF("findIdentityUser") {
      val userWithBanStatus = for {
        user <- OptionT(EitherT(userService.findById(id)))
        enrichedUser = userService.enrichUserWithBannedStatus(user).map(_.map(Option.apply))
        userWithBanStatus <- OptionT(EitherT(enrichedUser))
      } yield userWithBanStatus

      userWithBanStatus.run.run.map {
        case \/-(maybeUser) => maybeUser.fold[Result](NotFound)(user => Ok(user))
        case -\/(error) => InternalServerError(error)
      }
    }

  }

  def findSalesforceDetails(id: String) = auth.async { _ =>
    withMetricsF("findSalesforceDetails") {
      val subscriptionF = EitherT(salesforce.getSubscriptionByIdentityId(id))
      val membershipF = EitherT(salesforce.getMembershipByIdentityId(id))

      val salesForceDetails: DisjunctionT[Future, ApiError, SalesforceDetails] = for {
        subscription <- subscriptionF
        membership <- membershipF
      } yield SalesforceDetails(subscription, membership)

      salesForceDetails.run.map {
        case \/-(result) => Ok(result)
        case -\/(error) => InternalServerError(error)
      }
    }
  }

  def hasCommented(id: String) = auth.async { _ =>
    withMetricsF("hasCommented") {
      discussionService.hasCommented(id).map {
        case \/-(result) => Ok(result)
        case -\/(error) => InternalServerError(error)
      }
    }
  }

  def findExactTargetDetails(id: String) = auth.async { _ =>
    withMetricsF("findExactTargetDetails") {
      val exactTargetSubF = EitherT(exactTargetService.subscriberByIdentityId(id))
      val contributionsF = EitherT(exactTargetService.contributionsByIdentityId(id))

      val result = for {
        exactTargetSub <- exactTargetSubF
        contributions <- contributionsF
      } yield ExactTargetDetails(exactTargetSub, contributions)

      result.run.map {
        case \/-(result) => Ok(result)
        case -\/(error) => InternalServerError(error)
      }
    }
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
    logger.info("Unsubscribing from all editorial email lists ")

    EitherT(exactTargetService.unsubscribeFromAllLists(email)).fold(
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

  def validateEmail(id: String) = (auth andThen identityUserAction(id)).async { implicit request =>
    logger.info(s"Validating email for user with id: $id")
    Form[Boolean](single("validated" -> boolean)).bindFromRequest.fold(
      formWithErrors => Future(BadRequest(ApiError(s"Failed to validate email for user $id", formWithErrors.errors.toString))),
      validated => EitherT(userService.validateEmail(request.user.idapiUser, validated)).fold(
        error => InternalServerError(error),
        _ => NoContent
      )
    )
  }

  def userSubjectAccessRequest(id: String) = auth.async { implicit request =>
    logger.info(s"Getting subject access request for user with id: $id")
    withMetricsF("userSubjectAccessRequest") {
      val userSarResponse = userService.getSubjectAccessRequest(id)

      userSarResponse.map {
        case \/-(result) => {
          val fileToSend = Files.createTempFile(s"sar-$id-", ".txt").toFile
          val pw = new PrintWriter(fileToSend)
          result.foreach(pw.write)
          pw.close()
          Ok.sendFile(fileToSend, onClose = () => { fileToSend.delete() })
        }
        case -\/(error) => InternalServerError(error)
      }
    }
  }
}
