package controllers

import actions.AuthenticatedAction
import models.client.{ApiError, ReservedUsernameList}
import models.database.mongo.ReservedUserNameWriteRepository
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.libs.json.Json
import play.api.mvc.{BodyParsers, ControllerComponents, Request, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.concurrent.{ExecutionContext, Future}
import scalaz.{-\/, \/-}

class ReservedUsernameControllerTest extends WordSpec with Matchers with MockitoSugar with GuiceOneServerPerSuite {

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  val parser = app.injector.instanceOf[BodyParsers.Default]
  val cc = app.injector.instanceOf[ControllerComponents]

  val reservedUsernameRepo = mock[ReservedUserNameWriteRepository]

  // So we can apply FakeRequest without needing to add HMAC header
  class StubAuthenticatedAction(override val parser: BodyParsers.Default) extends AuthenticatedAction(parser) {
    override def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] = {
      block(request)
    }
  }

  val controller = new ReservedUsernameController(cc, reservedUsernameRepo, new StubAuthenticatedAction(parser))


  "reserveUsername" should {
    "return bad request when json cannot be parsed" in {
      val json = """{"key":"value"}"""
      val result = controller.reserveUsername()(FakeRequest().withBody(Json.parse(json)))
      status(result) shouldEqual BAD_REQUEST
    }

    "return no content when succesful" in {
      val json = """{"username":"usernameToReserve"}"""
      when(reservedUsernameRepo.addReservedUsername("usernameToReserve")).thenReturn(Future.successful(\/-(ReservedUsernameList())))
      val result = controller.reserveUsername()(FakeRequest().withBody(Json.parse(json)))
      status(result) shouldEqual NO_CONTENT
    }

    "return internal server error when error occurs" in {
      val json = """{"username":"usernameToReserve"}"""
      val error = ApiError("boom")
      when(reservedUsernameRepo.addReservedUsername("usernameToReserve")).thenReturn(Future.successful(-\/(error)))
      val result = controller.reserveUsername()(FakeRequest().withBody(Json.parse(json)))
      status(result) shouldEqual INTERNAL_SERVER_ERROR
      contentAsJson(result) shouldEqual Json.toJson(error)
    }
  }

  "unreserveUsername" should {
    "return bad request when json cannot be parsed" in {
      val json = """{"key":"value"}"""
      val result = controller.unreserveUsername()(FakeRequest().withBody(Json.parse(json)))
      status(result) shouldEqual BAD_REQUEST
    }

    "return no content when succesful" in {
      val json = """{"username":"usernameToReserve"}"""
      when(reservedUsernameRepo.removeReservedUsername("usernameToReserve")).thenReturn(Future.successful(\/-(ReservedUsernameList())))
      val result = controller.unreserveUsername()(FakeRequest().withBody(Json.parse(json)))
      status(result) shouldEqual NO_CONTENT
    }

    "return internal server error when error occurs" in {
      val json = """{"username":"usernameToReserve"}"""
      val error = ApiError("boom")
      when(reservedUsernameRepo.removeReservedUsername("usernameToReserve")).thenReturn(Future.successful(-\/(error)))
      val result = controller.unreserveUsername()(FakeRequest().withBody(Json.parse(json)))
      status(result) shouldEqual INTERNAL_SERVER_ERROR
      contentAsJson(result) shouldEqual Json.toJson(error)
    }
  }

  "getReservedUsernames" should {
    "return reserved usernames when successful" in {
      val reservedUsernameList = ReservedUsernameList(List("1", "2", "3"))
      when(reservedUsernameRepo.loadReservedUsernames).thenReturn(Future.successful(\/-(reservedUsernameList)))
      val result = controller.getReservedUsernames(FakeRequest())
      status(result) shouldEqual OK
      contentAsJson(result) shouldEqual Json.toJson(reservedUsernameList)
    }

    "return internal server error when error occurs" in {
      val error = ApiError("boom")
      when(reservedUsernameRepo.loadReservedUsernames).thenReturn(Future.successful(-\/(error)))
      val result = controller.getReservedUsernames(FakeRequest())
      status(result) shouldEqual INTERNAL_SERVER_ERROR
      contentAsJson(result) shouldEqual Json.toJson(error)
    }
  }
}
