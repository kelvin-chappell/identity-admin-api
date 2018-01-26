package actions

import models.client.ApiError
import org.joda.time.{DateTime, DateTimeZone}
import org.mockito.Mockito
import org.mockito.Mockito._
import org.scalatest.{BeforeAndAfter, Matchers, WordSpec}
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import util.Formats

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class AuthenticatedActionTest extends WordSpec with Matchers with BeforeAndAfter with GuiceOneServerPerSuite {

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  val parser = app.injector.instanceOf[BodyParsers.Default]
  val cc = app.injector.instanceOf[ControllerComponents]

  val action = spy(new AuthenticatedAction(parser))

  before {
    Mockito.reset(action)
  }

  "extractToken" should {
    "Return None if invalid format" in {
      val headerValue = "123456"
      HmacAuthenticator.extractToken(headerValue) shouldEqual None
    }

    "Return Some(token) from headerValue" in {
      val token = "xQh2D+jlEZh7tEBv0IwD10mklB5RRop665kRChbdZow="
      val headerValue = s"HMAC $token"

      HmacAuthenticator.extractToken(headerValue) shouldEqual Some(token)
    }
  }

  "invokeBlock" should {

    val block: Request[Any] => Future[Result] = request => Future.successful(Results.Ok)

    val dateHeaderValue = Formats.toHttpDateTimeString(DateTime.now)

    def verifyUnauthorized(result: Future[Result], errorMessage: String) = {
      status(result) shouldEqual UNAUTHORIZED
      contentAsJson(result) shouldEqual Json.toJson(ApiError("Authorization failure", errorMessage))
    }

    "Return unauthorised if Authorization header is missing" in {
      val request = FakeRequest().withHeaders(HeaderNames.AUTHORIZATION -> "HMAC 1234")
      val result = action.invokeBlock(request, block)
      verifyUnauthorized(result, "Date header is required.")
    }

    "Return unauthorised if Date header is missing" in {
      val request = FakeRequest().withHeaders(HeaderNames.DATE -> dateHeaderValue)
      val result = action.invokeBlock(request, block)
      verifyUnauthorized(result, "Authorization header is required.")
    }

    "Return unauthorised if HMAC token cannot be extracted from Authorization header" in {
      val authHeaderValue = "auth"
      val request = FakeRequest().withHeaders(HeaderNames.DATE -> dateHeaderValue, HeaderNames.AUTHORIZATION -> authHeaderValue)
      val result = action.invokeBlock(request, block)
      verifyUnauthorized(result, "Authorization header is invalid.")
    }

    "Return unauthorised if HMAC token does not match calculated HMAC" in {
      val authHeaderValue = "HMAC 12345"
      val path= "/v1/user/id?param=val"
      val request = FakeRequest("GET", path).withHeaders(HeaderNames.DATE -> dateHeaderValue, HeaderNames.AUTHORIZATION -> authHeaderValue)
      val result = action.invokeBlock(request, block)
      verifyUnauthorized(result, "Authorization token is invalid.")
    }

    "Return unauthorised if HMAC has expired" in {
      val path= "/v1/user/id?param=val"
      val dateHeader = Formats.toHttpDateTimeString(DateTime.now.plusMinutes(HmacAuthenticator.HmacValidDurationInMinutes + 1))
      val authHeaderValue = s"HMAC ${HmacAuthenticator.sign(dateHeaderValue, path)}"
      val request = FakeRequest("GET", path).withHeaders(HeaderNames.DATE -> dateHeader, HeaderNames.AUTHORIZATION -> authHeaderValue)
      val result = action.invokeBlock(request, block)
      verifyUnauthorized(result, "Authorization token is invalid.")
    }

    "Return unauthorised if Date cannot be parsed" in {
      val path= "/v1/user/id?param=val"
      val dateHeader = DateTime.now.withZone(DateTimeZone.forID("GMT")).toString("yyyy-MM-dd'T'HH:mm:ss")
      val authHeaderValue = s"HMAC ${HmacAuthenticator.sign(dateHeaderValue, path)}"
      val request = FakeRequest("GET", path).withHeaders(HeaderNames.DATE -> dateHeader, HeaderNames.AUTHORIZATION -> authHeaderValue)
      val result = action.invokeBlock(request, block)
      verifyUnauthorized(result, "Date header is of invalid format.")
    }

    "Execute block if auhorization header value matches calculated signed request" in {
      val path= "/v1/user/id?param=val"
      val authHeaderValue = s"HMAC ${HmacAuthenticator.sign(dateHeaderValue, path)}"
      val request = FakeRequest("GET", path).withHeaders(HeaderNames.DATE -> dateHeaderValue, HeaderNames.AUTHORIZATION -> authHeaderValue)
      Await.result(action.invokeBlock(request, block), 1.second) shouldEqual Results.Ok
    }

    "Execute block if HMAC was sent by client with skewed clock, but within time limit" in {
      val skewedDateHeader = Formats.toHttpDateTimeString(DateTime.now.minusMinutes(HmacAuthenticator.HmacValidDurationInMinutes - 1))
      val path= "/v1/user/id?param=val"
      val authHeaderValue = s"HMAC ${HmacAuthenticator.sign(skewedDateHeader, path)}"
      val request = FakeRequest("GET", path).withHeaders(HeaderNames.DATE -> skewedDateHeader, HeaderNames.AUTHORIZATION -> authHeaderValue)
      Await.result(action.invokeBlock(request, block), 1.second) shouldEqual Results.Ok
    }
  }
}
