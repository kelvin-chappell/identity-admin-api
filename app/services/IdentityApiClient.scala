package services

import com.gu.identity.model.{EmailNewsletter, EmailNewsletters}
import javax.inject.Inject
import com.typesafe.scalalogging.LazyLogging
import configuration.Config
import models.client.{ApiError, ApiResponse}
import play.api.libs.json._
import play.api.libs.ws.{WSClient, WSRequest}
import play.api.libs.functional.syntax._

import scala.concurrent.{ExecutionContext, Future}
import scalaz.{-\/, \/, \/-}

import scala.util.Try

case class IdentityApiError(message: String, description: String, context: Option[String] = None)

object IdentityApiError {
  implicit val format = Json.format[IdentityApiError]
}

case class IdentityApiErrorResponse(status: String, errors: List[IdentityApiError])

object IdentityApiErrorResponse {
  implicit val format = Json.format[IdentityApiErrorResponse]
}

class IdentityApiClient @Inject() (ws: WSClient)(implicit ec: ExecutionContext) extends LazyLogging {

  val baseUrl = Config.IdentityApi.baseUrl
  val clientToken = Config.IdentityApi.clientToken

  private val ClientTokenHeaderName = "X-GU-ID-Client-Access-Token"
  private lazy val clientTokenHeaderValue = s"Bearer $clientToken"
  
  private def sendEmailValidationUrl(userId: String): String = s"$baseUrl/user/$userId/send-validation-email"
  
  private def addAuthHeaders(req: WSRequest): WSRequest = {
    req.addHttpHeaders(ClientTokenHeaderName -> clientTokenHeaderValue)
  }

  def deleteUserById(userId: String): ApiResponse[Unit] = {
    val url = ws.url(s"$baseUrl/user/$userId")
    addAuthHeaders(url).delete().map {
      case response if response.status == 200 =>
        \/-(())
      case response =>
        val errorResponse = Json.parse(response.body).as[IdentityApiErrorResponse]
        val errorMessage = errorResponse.errors.headOption.map(x => x.message).getOrElse("Unknown error")
        -\/(ApiError(s"Failed to delete user $userId", errorMessage))
    }
  }

  def sendEmailValidation(userId: String): ApiResponse[Unit] = {
    addAuthHeaders(ws.url(sendEmailValidationUrl(userId))).post("").map(
      response => 
        if(response.status == 200)
            \/-{}
        else {
          val errorResponse = Json.parse(response.body).as[IdentityApiErrorResponse]
          val errorMessage = errorResponse.errors.headOption.map(x => x.message).getOrElse("Unknown error")
          -\/(ApiError("Send Email Validation Error", errorMessage))
        }
    ).recover { case e: Any =>
      val title = "Could not send email validation"
      logger.error(title, e.getMessage)
      -\/(ApiError(title, e.getMessage))
    }
  }

  private def newsletterFromId(id: String): Option[EmailNewsletter] = EmailNewsletters.all.subscriptions.find(_.allIds.exists(_.toString == id))

  def getUserLists(userId: String): ApiResponse[List[EmailNewsletter]] = {
    addAuthHeaders(ws.url(s"$baseUrl/useremails/$userId")).get().map { response =>
      val listIds = Try(Json.parse(response.body) \ "result" \ "subscriptions" match {
        case JsDefined(JsArray(values)) =>
          values.collect {
            case JsString(listId) => listId
          }
        case other =>
          logger.error(s"Unexpected format for subscriptions, expected a list of strings got $other")
          Seq.empty[String]
      }).getOrElse{
        logger.error(s"Failed to get list information for $userId")
        Seq.empty[String]
      }
      \/-(listIds.flatMap(listId => newsletterFromId(listId)).toList)
    }
  }

  def deleteAllLists(userId: String): ApiResponse[Unit] = {
    addAuthHeaders(ws.url(s"$baseUrl/useremails/$userId")).delete().map { response =>
      if (response.status == 200) {
        \/-(())
      } else {
        -\/(ApiError(s"Failed to delete subscriptions for $userId", s"Unexpected Response Code ${response.status}"))
      }
    }
  }

  def changeEmail(userId: String, newEmail: String): ApiResponse[Unit] = {
    addAuthHeaders(ws.url(s"$baseUrl/useremails/$userId/email")).post(
      Json.prettyPrint(JsObject(List("newEmail" -> JsString(newEmail))))
    ).map { response =>
      if (response.status == 200) {
        \/-(())
      } else {
        -\/(ApiError(s"Failed to change email for $userId", s"Unexpected Response Code ${response.status}"))
      }
    }
  }

}
