package controllers

import actions._
import mockws.MockWS
import models._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{Matchers, WordSpec}
import org.mockito.Matchers.any
import org.mockito.Mockito._
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.libs.json.Json
import play.api.mvc._
import play.api.mvc.Results._
import play.api.test.FakeRequest
import repositories.IdentityUser
import play.api.test.Helpers._
import services.{DiscussionService, ExactTargetService, SalesforceService, UserService}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.{-\/, \/-}

class UsersControllerTest extends WordSpec with Matchers with MockitoSugar with GuiceOneServerPerSuite {

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  val parser = app.injector.instanceOf[BodyParsers.Default]
  val cc = app.injector.instanceOf[ControllerComponents]

  val testIdentityId = "abc"

  val userService = mock[UserService]
  val dapiWsMockurl = s"/profile/10000001/stats"
  val dapiWsMock = MockWS { case (GET, dapiWsMockurl) => Action {Ok("""{"status":"ok","comments":0,"pickedComments":0}""")}}
  val exactTargetServiceMock = mock[ExactTargetService]
  val salesforceService = mock[SalesforceService]
  val identityUserAction = mock[IdentityUserAction]
  val orphanUserAction = mock[OrphanUserAction]

  when(exactTargetServiceMock.newslettersSubscriptionByIdentityId("abc")).thenReturn(Future.successful(\/-(None)))

  // So we can apply FakeRequest without needing to add HMAC header
  class StubAuthenticatedAction(override val parser: BodyParsers.Default) extends AuthenticatedAction(parser) {
    override def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] = {
      block(request)
    }
  }

  def createIdentityUserActionRightMock(user: GuardianUser) =
    new ActionRefiner[Request, UserRequest] {
      override def executionContext: ExecutionContext = ec
      override def refine[A](request: Request[A]): Future[Either[Result, UserRequest[A]]] =
        Future.successful(Right(new UserRequest(user, request)))
    }

  def createIdentityUserActionLeftMock() =
    new ActionRefiner[Request, UserRequest] {
      override def executionContext: ExecutionContext = ec
      override def refine[A](request: Request[A]): Future[Either[Result, UserRequest[A]]] =
        Future.successful(Left(NotFound))
    }

  val controller = new UsersController(
    cc,
    userService,
    new StubAuthenticatedAction(parser),
    identityUserAction,
    orphanUserAction,
    salesforceService,
    new DiscussionService(dapiWsMock),
    exactTargetServiceMock)


  "search" should {
    "return 400 when query string is less than minimum length" in {
      val query = "a"
      val limit = Some(10)
      val offset = Some(0)
      val result = controller.search(query, limit, offset).apply(FakeRequest().withHeaders())
      status(result) shouldEqual BAD_REQUEST
      contentAsJson(result) shouldEqual Json.toJson(ApiError("query must be a minimum of 2 characters"))
    }

    "return 400 when offset is negative" in {
      val query = "ab"
      val limit = Some(10)
      val offset = Some(-1)
      val result = controller.search(query, limit, offset)(FakeRequest())
      status(result) shouldEqual BAD_REQUEST
      contentAsJson(result) shouldEqual Json.toJson(ApiError("offset must be a positive integer"))
    }

    "return 400 when limit is negative" in {
      val query = "ab"
      val limit = Some(-10)
      val offset = Some(0)
      val result = controller.search(query, limit, offset)(FakeRequest())
      status(result) shouldEqual BAD_REQUEST
      contentAsJson(result) shouldEqual Json.toJson(ApiError("limit must be a positive integer"))
    }

    "return 200 when limit is not provided but search is successfully performed" in {
      val query = "test@test.com"
      val limit = None
      val offset = Some(0)
      val response = SearchResponse(0, hasMore = false, Nil)
      when(userService.search(query, limit, offset)).thenReturn(Future.successful(\/-(response)))
      val result = controller.search(query, limit, offset)(FakeRequest())
      status(result) shouldEqual OK
      contentAsJson(result) shouldEqual Json.toJson(response)
    }

    "return 200 and empty list when user not found" in {
      val query = "test@test.com"
      val limit = Some(10)
      val offset = Some(0)
      val response = SearchResponse(0, hasMore = false, Nil)
      when(userService.search(query, limit, offset)).thenReturn(Future.successful(\/-(response)))
      val result = controller.search(query, limit, offset)(FakeRequest())
      status(result) shouldEqual OK
      contentAsJson(result) shouldEqual Json.toJson(response)
    }
    
    "return 200 with user list as json when found" in {
      val email = "test@test.com"
      val query = email
      val user = IdentityUser(email, testIdentityId)
      val limit = Some(10)
      val offset = Some(0)
      val response = SearchResponse(10, hasMore = true, Seq(UserSummary.fromPersistedUser(user)))
      when(userService.search(query, limit, offset)).thenReturn(Future.successful(\/-(response)))
      val result = controller.search(query, limit, offset)(FakeRequest())
      status(result) shouldEqual OK
      contentAsJson(result) shouldEqual Json.toJson(response)
    }
    
  }

  "findById" should {
    "return 404 when user not found" in {
      when(identityUserAction.apply(any[String])).thenReturn(createIdentityUserActionLeftMock())
      when(userService.findById(testIdentityId)).thenReturn(Future.successful(\/-(None)))
      val result = controller.findById(testIdentityId)(FakeRequest())
      status(result) shouldEqual NOT_FOUND
    }

    "return 200 when user found" in {
      val user = GuardianUser(User(testIdentityId, "test@test.com"))
      when(identityUserAction.apply(any[String])).thenReturn(createIdentityUserActionRightMock(user))
      when(userService.findById(testIdentityId)).thenReturn(Future.successful(\/-(Some(user))))
      when(userService.enrichUserWithProducts(any[GuardianUser])).thenReturn(Future.successful(\/-(user)))
      val result = controller.findById(testIdentityId)(FakeRequest())
      status(result) shouldEqual OK
      contentAsJson(result) shouldEqual Json.toJson(user)
    }
  }

  "update" should {
    "return 400 when json is invalid" in {
      val json = """{"key":"value"}"""
      val result = controller.update(testIdentityId)(FakeRequest().withBody(Json.parse(json)))
      status(result) shouldEqual BAD_REQUEST
    }

    "return 404 when user is not found" in {
      val userUpdateRequest = UserUpdateRequest(email = "test@test.com", username = Some("username"))
      when(identityUserAction.apply(any[String])).thenReturn(createIdentityUserActionLeftMock())
      when(userService.findById(testIdentityId)).thenReturn(Future.successful(\/-(None)))
      val result = controller.update(testIdentityId)(FakeRequest().withBody(Json.toJson(userUpdateRequest)))
      status(result) shouldEqual NOT_FOUND
    }

    "return 400 when username and display name differ in request" in {
      val userUpdateRequest = UserUpdateRequest(email = "test@test.com", username = Some("username"), displayName = Some("displayname"))
      val user = User("id", "email")
      val guUser = GuardianUser(user)
      when(identityUserAction.apply(any[String])).thenReturn(createIdentityUserActionRightMock(guUser))
      when(userService.findById(testIdentityId)).thenReturn(Future.successful(\/-(Some(guUser))))
      when(userService.update(user, userUpdateRequest)).thenReturn(Future.successful(\/-(user)))
      val result = controller.update(testIdentityId)(FakeRequest().withBody(Json.toJson(userUpdateRequest)))
      status(result) shouldEqual BAD_REQUEST
    }

    "return 200 with updated user when update is successful" in {
      val userUpdateRequest = UserUpdateRequest(email = "test@test.com", username = Some("username"))
      val user = User("id", "email")
      val guUser = GuardianUser(user)
      when(userService.findById(testIdentityId)).thenReturn(Future.successful(\/-(Some(guUser))))
      when(userService.enrichUserWithProducts(any[GuardianUser])).thenReturn(Future.successful(\/-(guUser)))
      when(userService.update(user, userUpdateRequest)).thenReturn(Future.successful(\/-(user)))
      val result = controller.update(testIdentityId)(FakeRequest().withBody(Json.toJson(userUpdateRequest)))
      status(result) shouldEqual OK
      contentAsJson(result) shouldEqual Json.toJson(user)
    }
  }

  "delete" should {
    "return 404 when user is not found" in {
      when(identityUserAction.apply(any[String])).thenReturn(createIdentityUserActionLeftMock())
      when(userService.findById(testIdentityId)).thenReturn(Future.successful(\/-(None)))
      val result = controller.delete(testIdentityId)(FakeRequest())
      status(result) shouldEqual NOT_FOUND
    }
  }
  
  "sendEmailValidation" should {
    "return 404 when user is not found" in {
      when(identityUserAction.apply(any[String])).thenReturn(createIdentityUserActionLeftMock())
      when(userService.findById(testIdentityId)).thenReturn(Future.successful(\/-(None)))
      val result = controller.sendEmailValidation(testIdentityId)(FakeRequest())
      status(result) shouldEqual NOT_FOUND
    }

    "return 204 when email validation is sent" in {
      val user = GuardianUser(User("", ""))
      when(identityUserAction.apply(any[String])).thenReturn(createIdentityUserActionRightMock(user))
      when(userService.findById(testIdentityId)).thenReturn(Future.successful(\/-(Some(user))))
      when(userService.enrichUserWithProducts(any[GuardianUser])).thenReturn(Future.successful(\/-(user)))
      when(userService.sendEmailValidation(user.idapiUser)).thenReturn(Future.successful(\/-{}))
      val result = controller.sendEmailValidation(testIdentityId)(FakeRequest())
      status(result) shouldEqual NO_CONTENT
    }

    "return 500 when error occurs" in {
      val user = GuardianUser(User("", ""))
      when(identityUserAction.apply(any[String])).thenReturn(createIdentityUserActionRightMock(user))
      when(userService.findById(testIdentityId)).thenReturn(Future.successful(\/-(Some(user))))
      when(userService.sendEmailValidation(user.idapiUser)).thenReturn(Future.successful(-\/(ApiError("boom"))))
      val result = controller.sendEmailValidation(testIdentityId)(FakeRequest())
      status(result) shouldEqual INTERNAL_SERVER_ERROR
      contentAsJson(result) shouldEqual Json.toJson(ApiError("boom"))
    }
  }

  "validateEmail" should {
    "return 404 when user is not found" in {
      when(identityUserAction.apply(any[String])).thenReturn(createIdentityUserActionLeftMock())
      when(userService.findById(testIdentityId)).thenReturn(Future.successful(\/-(None)))
      val result = controller.validateEmail(testIdentityId)(FakeRequest())
      status(result) shouldEqual NOT_FOUND
    }

    "return 204 when email is validated" in {
      val user = GuardianUser(User("", ""))
      when(identityUserAction.apply(any[String])).thenReturn(createIdentityUserActionRightMock(user))
      when(userService.findById(testIdentityId)).thenReturn(Future.successful(\/-(Some(user))))
      when(userService.validateEmail(user.idapiUser)).thenReturn(Future.successful(\/-{}))
      val result = controller.validateEmail(testIdentityId)(FakeRequest())
      status(result) shouldEqual NO_CONTENT
    }

    "return 500 when error occurs" in {
      val user = GuardianUser(User("", ""))
      when(identityUserAction.apply(any[String])).thenReturn(createIdentityUserActionRightMock(user))
      when(userService.findById(testIdentityId)).thenReturn(Future.successful(\/-(Some(user))))
      when(userService.validateEmail(user.idapiUser)).thenReturn(Future.successful(-\/(ApiError("boom"))))
      val result = controller.validateEmail(testIdentityId)(FakeRequest())
      status(result) shouldEqual INTERNAL_SERVER_ERROR
      contentAsJson(result) shouldEqual Json.toJson(ApiError("boom"))
    }
  }
}
