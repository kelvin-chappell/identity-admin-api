package actions

import models.client.ClientJsonFormats._
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import javax.inject._

import com.typesafe.scalalogging.LazyLogging
import configuration.Config
import org.apache.commons.codec.binary.Base64
import org.joda.time.{DateTime, DateTimeZone}
import play.api.http.HeaderNames
import play.api.mvc.{Request, Result, _}
import play.api.mvc.Results._
import util.Formats

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import play.api.mvc._
import models.client.ApiError

object HmacSigner {
  def sign(date: String, path: String, secret: String): String = {
    val input = List[String](date, path)
    val toSign = input.mkString("\n")
    calculateHMAC(toSign, secret)
  }

  private def calculateHMAC(toEncode: String, secret: String): String = {
    val Algorithm = "HmacSHA256"
    val signingKey = new SecretKeySpec(secret.getBytes, Algorithm)
    val mac = Mac.getInstance(Algorithm)
    mac.init(signingKey)
    val rawHmac = mac.doFinal(toEncode.getBytes)
    new String(Base64.encodeBase64(rawHmac))
  }
}

object HmacAuthenticator extends LazyLogging {
  val secret = Config.hmacSecret

  val HmacPattern = "HMAC\\s(.+)".r
  val HmacValidDurationInMinutes = 5
  val MinuteInMilliseconds = 60000

  def isDateValid(date: String): Boolean  = {
    val provided = Try(Formats.toDateTime(date)) match {
      case Success(parsedDate) => parsedDate
      case _ =>
        logger.error(s"Could not parse date header from HTTP headers: $date")
        throw new scala.IllegalArgumentException("Date header is of invalid format.")
    }
    val now = DateTime.now(DateTimeZone.forID("GMT"))
    val delta = Math.abs(provided.getMillis - now.getMillis)
    val allowedOffset = HmacValidDurationInMinutes * MinuteInMilliseconds
    delta <= allowedOffset
  }

  def isHmacValid(date: String, uri: String, hmac: String): Boolean = {
    val hmacWithoutQuotes = hmac.replaceAll("\"", "") // Because https://github.com/akka/akka-http/issues/966
    sign(date, uri) == hmacWithoutQuotes
  }

  def extractToken(authHeader: String): Option[String] = {
    (HmacPattern.findAllIn(authHeader).matchData map { m => m.group(1) }).toList.headOption
  }

  def sign(date: String, path: String): String = HmacSigner.sign(date, path, secret)
}

class AuthenticatedAction @Inject() (val parser: BodyParsers.Default)(implicit val executionContext: ExecutionContext)
    extends ActionBuilder[Request, AnyContent] with LazyLogging {

  def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] = {
    Try {
      val authorization = request.headers.get(HeaderNames.AUTHORIZATION).getOrElse(throw new IllegalArgumentException("Authorization header is required."))
      val hmac = HmacAuthenticator.extractToken(authorization).getOrElse(throw new IllegalArgumentException("Authorization header is invalid."))
      val date = request.headers.get(HeaderNames.DATE).getOrElse(throw new scala.IllegalArgumentException("Date header is required."))
      val uri = request.uri

      logger.trace(s"path: $uri, date: $date, hmac: $hmac")

      if (HmacAuthenticator.isDateValid(date) && HmacAuthenticator.isHmacValid(date, uri, hmac)) {
        Success
      } else {
        throw new IllegalArgumentException("Authorization token is invalid.")
      }
    } match {
      case Success(r) => block(request)
      case Failure(error) =>
        Future.successful(Unauthorized(ApiError("Authorization failure", error.getMessage)))
    }
  }
}
