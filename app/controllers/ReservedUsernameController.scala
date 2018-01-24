package controllers

import models.client.ClientJsonFormats._
import javax.inject.{Inject, Singleton}

import actions.AuthenticatedAction
import com.gu.identity.util.Logging
import models.client.ApiError
import models.database.mongo.ReservedUserNameWriteRepository
import play.api.libs.json.{JsError, JsSuccess, Json}
import play.api.mvc.{AbstractController, ControllerComponents}

import scalaz.EitherT
import scalaz.std.scalaFuture._
import scala.concurrent.{ExecutionContext, Future}

case class ReservedUsernameRequest(username: String)

object ReservedUsernameRequest {
  implicit val format = Json.format[ReservedUsernameRequest]
}

@Singleton
class ReservedUsernameController @Inject() (
    cc: ControllerComponents,
    reservedUsernameRepository: ReservedUserNameWriteRepository,
    auth: AuthenticatedAction)(implicit ec: ExecutionContext) extends AbstractController(cc) with Logging {

  def reserveUsername() = auth.async(parse.json) { request =>
    request.body.validate[ReservedUsernameRequest] match {
      case JsSuccess(result, path) =>
        logger.info(s"Reserving username: ${result.username}")
        EitherT(reservedUsernameRepository.addReservedUsername(result.username)).fold(
          error => InternalServerError(error),
          _ => NoContent
        )
      case JsError(e) => Future.successful(BadRequest(ApiError("Failed to reserve username", e.toString)))
    }
  }

  def getReservedUsernames = auth.async { request =>
    EitherT(reservedUsernameRepository.loadReservedUsernames).fold(
      error => InternalServerError(error),
      success => Ok(Json.toJson(success))
    )
  }

  def unreserveUsername() = auth.async(parse.json) { request =>
    request.body.validate[ReservedUsernameRequest] match {
      case JsSuccess(result, path) =>
        logger.info(s"Unreserving username: ${result.username}")
        EitherT(reservedUsernameRepository.removeReservedUsername(result.username)).fold(
          error => InternalServerError(error),
          _ => NoContent
        )
      case JsError(error) => Future.successful(BadRequest(ApiError("Failed to unreserve username", error.toString)))
    }
  }

}
