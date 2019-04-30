package services

import com.gu.identity.model.{EmailNewsletter, EmailNewsletters}
import javax.inject.Inject
import com.typesafe.scalalogging.LazyLogging
import configuration.Config
import models.client.{ApiError, ApiResponse, NewslettersSubscription}
import play.api.libs.json._
import play.api.libs.ws.{WSClient, WSRequest}
import play.api.libs.functional.syntax._
import scala.concurrent.{ExecutionContext, Future}
import scalaz.{-\/, \/, \/-}
import scala.util.{Success, Try}
case class IdentityApiError(message: String, description: String, context: Option[String] = None)

object IdentityApiError {
  implicit val format = Json.format[IdentityApiError]
}

case class IdentityApiErrorResponse(status: String, errors: List[IdentityApiError])

object IdentityApiErrorResponse {
  implicit val format = Json.format[IdentityApiErrorResponse]
}

case class IdentityNewsletterListId(listId: String)
case class IdentityNewsletterSubs(globalSubscriptionStatus: String, subscriptions: List[IdentityNewsletterListId])
case class IdentityNewsletterResponse(result: IdentityNewsletterSubs)

object IdentityNewsletterResponse {
  implicit val listFormat = Json.format[IdentityNewsletterListId]
  implicit val subsFormat = Json.format[IdentityNewsletterSubs]
  implicit val newsletterResponseFormat = Json.format[IdentityNewsletterResponse]
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

  def findNewsletterSubscriptions(userId: String): ApiResponse[NewslettersSubscription] = {
    addAuthHeaders(ws.url(s"$baseUrl/useremails/$userId")).get().map { response =>
      Try(Json.parse(response.body).as[IdentityNewsletterResponse]) match {
        case Success(parsedResponse) =>
          \/-(NewslettersSubscription(parsedResponse.result.globalSubscriptionStatus, parsedResponse.result.subscriptions.map(_.listId)))
        case _ =>
          logger.error(s"findNewsletterSubscriptions unexpected response from idapi $userId ${response.status} ${response.body}")
          -\/(ApiError("unexpected response from idapi"))
      }

    }
  }

  // TODO: it seems that not all these methods have a way of recovering from a failed Future
  def unsubscribeAll(userId: String): ApiResponse[Unit] = {
    addAuthHeaders(ws.url(s"$baseUrl/useremails/$userId/unsubscribe")).post("").map { response =>
      if (response.status == 200) {
        \/-(())
      } else {
        -\/(ApiError(s"Failed to delete subscriptions for $userId", s"Unexpected Response Code $userId ${response.status} ${response.body}"))
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
        -\/(ApiError(s"Failed to change email for $userId", s"Unexpected Response Code $userId ${response.status} ${response.body}"))
      }
    }
  }

  def removeFromBounceList(userId: String): ApiResponse[Unit] = {
    addAuthHeaders(ws.url(s"$baseUrl/useremails/$userId/bounce-list-remove")).post("").map { response =>
      if (response.status == 200) {
        \/-(())
      } else {
        -\/(ApiError("unexpected response from idapi", s"${response.status}, ${response.body}"))
      }
    }.recover { case e =>
        logger.error("Could not remove user from hard bounce list", e.getMessage)
      -\/(ApiError("Could not remove user from hard bounce list", e.getMessage))
    }
  }

  def removeFromSpamList(userId: String): ApiResponse[Unit] = {
    addAuthHeaders(ws.url(s"$baseUrl/useremails/$userId/spam-list-remove")).post("").map { response =>
      if (response.status == 200) {
        \/-(())
      } else {
        -\/(ApiError("unexpected response from idapi", s"${response.status}, ${response.body}"))
      }
    }.recover { case e =>
      logger.error("Could not remove user from spam list", e.getMessage)
      -\/(ApiError("Could not remove user from spam list", e.getMessage))
    }
  }
}
