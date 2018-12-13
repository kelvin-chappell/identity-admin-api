package services

import actors.{EventPublishingActorProvider, MetricsActorProviderStub}
import akka.actor.ActorSystem
import models.client._
import models.database.postgres._
import util.UserConverter._
import org.mockito.Mockito
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, Matchers, WordSpec}
import org.scalatest.mockito.MockitoSugar

import scala.concurrent.{Await, ExecutionContext, Future}
import org.mockito.Mockito._
import org.scalatestplus.play.guice.GuiceOneServerPerSuite

import scala.concurrent.duration._
import scalaz.{-\/, \/-}

class UserServiceTest extends WordSpec with MockitoSugar with Matchers with BeforeAndAfter with GuiceOneServerPerSuite with BeforeAndAfterAll {

  implicit val ec = app.injector.instanceOf[ExecutionContext]

  val jobsGroup = Seq(UserGroup( "GRS", "/sys/policies/guardian-jobs"))

  val identityApiClient = mock[IdentityApiClient]
  val eventPublishingActorProvider = mock[EventPublishingActorProvider]
  val salesforceService = mock[SalesforceService]
  val salesforceIntegration = mock[SalesforceIntegration]
  val madgexService = mock[MadgexService]
  val exactTargetService = mock[ExactTargetService]
  val discussionService = mock[DiscussionService]
  val pgDeletedUserRepo = mock[PostgresDeletedUserRepository]
  val pgUserRepo = mock[PostgresUserRepository]
  val pgReservedUsernameRepo = mock[PostgresReservedUsernameRepository]
  val pgReservedEmailRepo = mock[PostgresReservedEmailRepository]
  val postgresSubjectAccessRequestRepository = mock[PostgresSubjectAccessRequestRepository]
  val brazeCmtService = mock[BrazeCmtService]
  implicit val actorSystem = ActorSystem()

  override def afterAll: Unit = {
    actorSystem.terminate()
  }

  val service =
    spy(new UserService(identityApiClient,
      eventPublishingActorProvider, salesforceService, salesforceIntegration, madgexService, exactTargetService,
      discussionService, pgDeletedUserRepo, pgReservedUsernameRepo, pgReservedEmailRepo, pgUserRepo, postgresSubjectAccessRequestRepository, MetricsActorProviderStub, brazeCmtService))

  before {
    Mockito.reset(identityApiClient, eventPublishingActorProvider, service, madgexService, pgDeletedUserRepo, pgReservedUsernameRepo, pgReservedEmailRepo, pgUserRepo)
  }

  "isUsernameChanged" should {
    "return true if a username is being changed" in {
      service.isUsernameChanged(Some("changedUsername"), Some("oldUsername")) should be(true)
    }

    "return false if a username is not being changed" in {
      service.isUsernameChanged(Some("oldUsername"), Some("oldUsername")) should be(false)
    }

    "return true if a username is being added" in {
      service.isUsernameChanged(Some("newUsername"), None) should be(true)
    }

    "return false if a zero length username is being added" in {
      service.isUsernameChanged(None, None) should be(false)
      service.isUsernameChanged(None, Some("existingUsername")) should be(true)
    }
  }

  "isDisplayNameChanged" should {
    "return true if a username is being changed" in {
      service.isDisplayNameChanged(Some("changedDisplayName"), Some("oldDisplayName")) should be(true)
    }

    "return false if a username is not being changed" in {
      service.isDisplayNameChanged(Some("oldDisplayName"), Some("oldDisplayName")) should be(false)
    }

    "return true if a username is being added" in {
      service.isDisplayNameChanged(Some("changedDisplayName"), None) should be(true)
    }

    "return false if a zero length username is being added" in {
      service.isDisplayNameChanged(None, None) should be(false)
      service.isDisplayNameChanged(None, Some("existingDisplayName")) should be(true)
    }
  }

  "isJobsUser" should {
    "return true for a jobs user" in {
      val user = User("id", "email@theguardian.com", groups = jobsGroup)
      service.isJobsUser(user) should be(true)
    }

    "return true for a non-jobs user" in {
      val user = User("id", "email@theguardian.com")
      service.isJobsUser(user) should be(false)
    }
  }

  "isEmailValidationChanged" should {
    "return true if e-mail validation status is changing" in {
      service.isEmailValidationChanged(Some(false), Some(true)) should be(true)
      service.isEmailValidationChanged(Some(false), None) should be(true)
      service.isEmailValidationChanged(Some(true), Some(false)) should be(true)
      service.isEmailValidationChanged(Some(true), None) should be(true)
      service.isEmailValidationChanged(None, Some(true)) should be(true)
      service.isEmailValidationChanged(None, Some(false)) should be(true)
    }

    "return false if e-mail validation status is not changing" in {
      service.isEmailValidationChanged(None, None) should be(false)
      service.isEmailValidationChanged(Some(true), Some(true)) should be(false)
      service.isEmailValidationChanged(Some(false), Some(false)) should be(false)
    }
  }

  "update" should {
    "update when email and username are valid" in {
      val user = User("id", "email@theguardian.com")
      val userUpdateRequest = UserUpdateRequest(email = "changedEmail@theguardian.com", username = Some("username"))

      val updatedUser = user.copy(email = userUpdateRequest.email)

      when(pgUserRepo.update(user, userUpdateRequest)).thenReturn(Future.successful(\/-(updatedUser)))
      when(identityApiClient.sendEmailValidation(user.id)).thenReturn(Future.successful(\/-{}))
      when(pgReservedEmailRepo.isReserved(userUpdateRequest.email)).thenReturn(Future.successful(\/-(false)))

      val result = service.update(user, userUpdateRequest)

      Await.result(result, 1.second) shouldEqual \/-(updatedUser)
      verify(identityApiClient).sendEmailValidation(user.id)
    }

    "not update when email address is reserved" in {
      val user = User("id", "email@theguardian.com")
      val userUpdateRequest = UserUpdateRequest(email = "changedEmail@theguardian.com", username = Some("username"))

      when(pgReservedEmailRepo.isReserved(userUpdateRequest.email)).thenReturn(Future.successful(\/-(true)))

      val result = service.update(user, userUpdateRequest)

      Await.result(result, 1.second) shouldEqual -\/(ApiError("Email is reserved"))
    }

    "update madgex when jobs user changed" in {
      val user = User("id", "email@theguardian.com", groups = jobsGroup)
      val userUpdateRequest = UserUpdateRequest(email = "changedEmail@theguardian.com")
      val gNMMadgexUser = GNMMadgexUser(user.id, userUpdateRequest)
      
      val updatedUser = user.copy(email = userUpdateRequest.email)

      when(pgUserRepo.update(user, userUpdateRequest)).thenReturn(Future.successful(\/-(updatedUser)))
      when(identityApiClient.sendEmailValidation(user.id)).thenReturn(Future.successful(\/-{}))
      when(pgReservedEmailRepo.isReserved(userUpdateRequest.email)).thenReturn(Future.successful(\/-(false)))

      val result = service.update(user, userUpdateRequest)

      Await.result(result, 1.second) shouldEqual \/-(updatedUser)
      verify(madgexService).update(gNMMadgexUser)
    }

    "not update madgex when non-jobs user changed" in {
      val user = User("id", "email@theguardian.com")
      val userUpdateRequest = UserUpdateRequest(email = "changedEmail@theguardian.com")
      
      val updatedUser = user.copy(email = userUpdateRequest.email)

      when(pgUserRepo.update(user, userUpdateRequest)).thenReturn(Future.successful(\/-(updatedUser)))
      when(pgReservedEmailRepo.isReserved(userUpdateRequest.email)).thenReturn(Future.successful(\/-(false)))

      Await.result(service.update(user, userUpdateRequest), 1.second) shouldEqual \/-(updatedUser)
      verifyZeroInteractions(madgexService)
    }

    "not send email validation when email has not changed" in {
      val user = User("id", "email@theguardian.com")
      val userUpdateRequest = UserUpdateRequest(email = "email@theguardian.com", username = Some("username"))
      
      val updatedUser = user.copy(email = userUpdateRequest.email)

      when(pgUserRepo.update(user, userUpdateRequest)).thenReturn(Future.successful(\/-(updatedUser)))
      when(pgReservedEmailRepo.isReserved(userUpdateRequest.email)).thenReturn(Future.successful(\/-(false)))

      Await.result(service.update(user, userUpdateRequest), 1.second) shouldEqual \/-(updatedUser)
      verifyZeroInteractions(identityApiClient)
    }

    "not update madgex when jobs user as not changed" in {
      val user = User("id", "email@theguardian.com")
      val userUpdateRequest = UserUpdateRequest(email = "email@theguardian.com", username = Some("username"))

      val updatedUser = user.copy(email = userUpdateRequest.email)

      when(pgUserRepo.update(user, userUpdateRequest)).thenReturn(Future.successful(\/-(updatedUser)))
      when(pgReservedEmailRepo.isReserved(userUpdateRequest.email)).thenReturn(Future.successful(\/-(false)))

      val result = service.update(user, userUpdateRequest)

      Await.result(result, 1.second) shouldEqual \/-(updatedUser)
      verifyZeroInteractions(madgexService)
    }

    "return bad request api error if the username is less than 6 chars" in {
      val user = User("id", "email@theguardian.com")
      val updateRequest = UserUpdateRequest(email = user.email, username = Some("123"))

      when(pgReservedEmailRepo.isReserved(updateRequest.email)).thenReturn(Future.successful(\/-(false)))

      Await.result(service.update(user, updateRequest), 1.second) shouldEqual -\/(ApiError("Username is invalid"))
      verifyZeroInteractions(pgUserRepo)
    }

    "return bad request api error if the username is more than 20 chars" in {
      val user = User("id", "email@theguardian.com")
      val updateRequest = UserUpdateRequest(email = user.email, username = Some("123456789012345678901"))

      when(pgReservedEmailRepo.isReserved(updateRequest.email)).thenReturn(Future.successful(\/-(false)))

      Await.result(service.update(user, updateRequest), 1.second) shouldEqual -\/(ApiError("Username is invalid"))
      verifyZeroInteractions(pgUserRepo)
    }

    "return bad request api error if the username is contains non alpha-numeric chars" in {
      val user = User("id", "email@theguardian.com")
      val updateRequest = UserUpdateRequest(email = user.email, username = Some("abc123$"))

      when(pgReservedEmailRepo.isReserved(updateRequest.email)).thenReturn(Future.successful(\/-(false)))

      Await.result(service.update(user, updateRequest), 1.second) shouldEqual -\/(ApiError("Username is invalid"))
      verifyZeroInteractions(pgUserRepo)
    }


    "return internal server api error if an error occurs updating the user" in {
      val user = User("id", "email@theguardian.com")
      val userUpdateRequest = UserUpdateRequest(email = "email@theguardian.com", username = Some("username"))
      

      when(pgUserRepo.update(user, userUpdateRequest)).thenReturn(Future.successful(-\/(ApiError("boom"))))
      when(pgReservedEmailRepo.isReserved(userUpdateRequest.email)).thenReturn(Future.successful(\/-(false)))

      Await.result(service.update(user, userUpdateRequest), 1.second) shouldEqual -\/(ApiError("boom"))
      verify(identityApiClient, never()).sendEmailValidation(user.id)
    }
  }

  "delete" should {
    "remove the given user and reserve username" in {
      val username = "testuser"
      val user = GuardianUser(User("id", "email", username = Some(username)))
      when(identityApiClient.deleteUserById(user.idapiUser.id)).thenReturn(Future.successful(\/-(())))
      when(pgReservedUsernameRepo.addReservedUsername(username)).thenReturn(Future.successful(\/-(ReservedUsernameList(List(username)))))
      Await.result(service.delete(user), 1.second) shouldEqual \/-(ReservedUsernameList(List(username)))
    }

    "remove the given user and return existing reserved usernames when user has no username" in {
      val user = GuardianUser(User("id", "email", username = None))
      when(identityApiClient.deleteUserById(user.idapiUser.id)).thenReturn(Future.successful(\/-(())))
      when(pgReservedUsernameRepo.loadReservedUsernames).thenReturn(Future.successful(\/-(ReservedUsernameList(Nil))))
      Await.result(service.delete(user), 1.second) shouldEqual \/-(ReservedUsernameList(Nil))
    }

    "return internal server api error if an error occurs deleting the user" in {
      val user = GuardianUser(User("id", "email"))
      when(identityApiClient.deleteUserById(user.idapiUser.id)).thenReturn(Future.successful(-\/(ApiError("boom"))))
      Await.result(service.delete(user), 1.second) shouldEqual -\/(ApiError("boom"))
    }
  }

  "validateEmailAddress" should {
    "validate the email address" in {
      val user = User("id", "email")
      when(pgUserRepo.updateEmailValidationStatus(user, true)).thenReturn(Future.successful(\/-(user)))
      Await.result(service.validateEmail(user), 1.second) shouldEqual \/-{}
    }

    "return internal server api error if an error occurs validating the email address" in {
      val user = User("id", "email")
      when(pgUserRepo.updateEmailValidationStatus(user, true)).thenReturn(Future.successful(-\/(ApiError("boom"))))

      Await.result(service.validateEmail(user), 1.second) shouldEqual -\/(ApiError("boom"))
    }
  }

  "sendEmailValidation" should {
    "invalidate the email address and send validation request email" in {
      val user = User("id", "email")
      when(pgUserRepo.updateEmailValidationStatus(user, false)).thenReturn(Future.successful(\/-(user)))
      when(identityApiClient.sendEmailValidation(user.id)).thenReturn(Future.successful(\/-{}))
      Await.result(service.sendEmailValidation(user), 1.second) shouldEqual \/-{}
    }

    "return internal server api error if an error occurs invalidating the email address" in {
      val user = User("id", "email")
      when(pgUserRepo.updateEmailValidationStatus(user, false)).thenReturn(Future.successful(-\/(ApiError("boom"))))

      Await.result(service.sendEmailValidation(user), 1.second) shouldEqual -\/(ApiError("boom"))
      verifyZeroInteractions(identityApiClient)
    }

    "return internal server api error if an error occurs sending the email" in {
      val user = User("id", "email")
      when(pgUserRepo.updateEmailValidationStatus(user, false)).thenReturn(Future.successful(\/-(user)))
      when(identityApiClient.sendEmailValidation(user.id)).thenReturn(Future.successful(-\/(ApiError("boom"))))

      Await.result(service.sendEmailValidation(user), 1.second) shouldEqual -\/(ApiError("boom"))
    }
  }

}
