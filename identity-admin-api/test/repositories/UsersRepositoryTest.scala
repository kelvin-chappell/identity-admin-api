package repositories

import java.util.UUID

import models.{User, UserUpdateRequest, SearchResponse}
import org.scalatest.DoNotDiscover
import org.scalatestplus.play.{OneServerPerSuite, PlaySpec}
import play.api.Play
import reactivemongo.bson.BSONObjectID

import scala.concurrent.Await
import scala.concurrent.duration._

@DoNotDiscover
class UsersRepositoryTest extends PlaySpec with OneServerPerSuite {

  def createUser(username: Option[String] = None, postcode: Option[String] = None, registeredIp: Option[String] = None, lastActiveIp: Option[String] = None): PersistedUser = {
    val email = s"${UUID.randomUUID().toString}@test.com"
    PersistedUser(email,
      Some(BSONObjectID.generate.toString()),
      publicFields = Some(PublicFields(username = username)),
      privateFields = Some(
        PrivateFields(postcode = postcode,
        registrationIp = registeredIp,
        lastActiveIpAddress = lastActiveIp)))
  }

    "search" must {

      "return a user when email matches exactly" in {
        val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])
        val writeRepo = Play.current.injector.instanceOf(classOf[UsersWriteRepository])
        val user = createUser()
        val createdUser = writeRepo.createUser(user)
        Await.result(repo.search(user.primaryEmailAddress), 1.second).results.map(_.id) must contain(createdUser.get)
      }

      "return a user when username matches exactly" in {
        val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])
        val writeRepo = Play.current.injector.instanceOf(classOf[UsersWriteRepository])
        val username = UUID.randomUUID().toString
        val user = createUser(username = Some(username))
        val createdUser = writeRepo.createUser(user)
        Await.result(repo.search(username), 1.second).results.map(_.id) must contain(createdUser.get)
      }

      "return a user when postcode matches exactly" in {
        val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])
        val writeRepo = Play.current.injector.instanceOf(classOf[UsersWriteRepository])
        val postcode = "N1 9GU"
        val user = createUser(postcode = Some(postcode))
        val createdUser = writeRepo.createUser(user)
        Await.result(repo.search(postcode), 1.second).results.map(_.id) must contain(createdUser.get)
      }

      "return a user when registered ip matches exactly" in {
        val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])
        val writeRepo = Play.current.injector.instanceOf(classOf[UsersWriteRepository])
        val ip = "127.0.0.1"
        val user = createUser(registeredIp = Some(ip))
        val createdUser = writeRepo.createUser(user)
        Await.result(repo.search(ip), 1.second).results.map(_.id) must contain(createdUser.get)
      }

      "return a user when last active ip matches exactly" in {
        val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])
        val writeRepo = Play.current.injector.instanceOf(classOf[UsersWriteRepository])
        val ip = "127.0.0.1"
        val user = createUser(lastActiveIp = Some(ip))
        val createdUser = writeRepo.createUser(user)
        Await.result(repo.search(ip), 1.second).results.map(_.id) must contain(createdUser.get)
      }
      
      "return Nil when no results are found" in {
        val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])
        Await.result(repo.search("invalid@invalid.com"), 1.second) mustEqual SearchResponse(0, hasMore = false, Nil)
      }

      "use offset when provided" in {
        val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])
        val writeRepo = Play.current.injector.instanceOf(classOf[UsersWriteRepository])
        val postcode = UUID.randomUUID().toString
        val user1 = createUser(postcode = Some(postcode))
        val user2 = createUser(postcode = Some(postcode))
        val user3 = createUser(postcode = Some(postcode))
        val user4 = createUser(postcode = Some(postcode))
        val user5 = createUser(postcode = Some(postcode))

        val createdUser1 = writeRepo.createUser(user1)
        val createdUser2 = writeRepo.createUser(user2)
        val createdUser3 = writeRepo.createUser(user3)
        val createdUser4 = writeRepo.createUser(user4)
        val createdUser5 = writeRepo.createUser(user5)

        val ids = Await.result(repo.search(postcode, offset = Some(1)), 1.second).results.map(_.id)
        ids must not contain createdUser1.get
        ids must contain(createdUser2.get)
        ids must contain(createdUser3.get)
        ids must contain(createdUser4.get)
        ids must contain(createdUser5.get)
      }

      "use limit when provided" in {
        val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])
        val writeRepo = Play.current.injector.instanceOf(classOf[UsersWriteRepository])
        val postcode = UUID.randomUUID().toString
        val user1 = createUser(postcode = Some(postcode))
        val user2 = createUser(postcode = Some(postcode))
        val user3 = createUser(postcode = Some(postcode))
        val user4 = createUser(postcode = Some(postcode))
        val user5 = createUser(postcode = Some(postcode))

        val createdUser1 = writeRepo.createUser(user1)
        val createdUser2 = writeRepo.createUser(user2)
        val createdUser3 = writeRepo.createUser(user3)
        val createdUser4 = writeRepo.createUser(user4)
        val createdUser5 = writeRepo.createUser(user5)

        val ids = Await.result(repo.search(postcode, offset = Some(1), limit = Some(2)), 1.second).results.map(_.id)
        ids.size mustEqual 2
        ids must contain(createdUser2.get)
        ids must contain(createdUser3.get)
      }

  }

  "findById" should {
    "return Some user when user found" in {
      val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])
      val writeRepo = Play.current.injector.instanceOf(classOf[UsersWriteRepository])
      val user1 = createUser()
      val createdUser1 = writeRepo.createUser(user1)

      Await.result(repo.findById(createdUser1.get), 1.second).map(_.id) mustEqual createdUser1
    }

    "return None when user not found" in {
      val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])

      Await.result(repo.findById(UUID.randomUUID().toString), 1.second) mustEqual None
    }
  }
  
  "findByEmail" should {
    "return Some user when user found" in {
      val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])
      val writeRepo = Play.current.injector.instanceOf(classOf[UsersWriteRepository])
      val user1 = createUser()
      val createdUser1 = writeRepo.createUser(user1)

      Await.result(repo.findByEmail(user1.primaryEmailAddress), 1.second).map(_.id) mustEqual createdUser1
    }

    "return None when user not found" in {
      val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])

      Await.result(repo.findByEmail(UUID.randomUUID().toString), 1.second) mustEqual None
    }
  }

  "update" should {
    "persist fields" in {
      val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])
      val writeRepo = Play.current.injector.instanceOf(classOf[UsersWriteRepository])
      val user1 = createUser()
      val createdUser1 = writeRepo.createUser(user1)
      val userUpdateRequest = UserUpdateRequest(
        email = UUID.randomUUID().toString,
        username = UUID.randomUUID().toString,
        firstName = Some("firstName"),
        lastName = Some("lastName"),
        receiveGnmMarketing = Some(true),
        receive3rdPartyMarketing = Some(false))

      val updateRequest = PersistedUserUpdate(userUpdateRequest, Some(false))

      val origUser = User.fromPersistedUser(user1.copy(_id = createdUser1))

      val result  = writeRepo.update(origUser, updateRequest)
      result.isRight mustBe true

      val updatedUser = Await.result(repo.findById(createdUser1.get), 1.second).get
      updatedUser.email mustEqual userUpdateRequest.email
      updatedUser.username mustEqual Some(userUpdateRequest.username)
      updatedUser.displayName mustEqual Some(userUpdateRequest.username)
      updatedUser.vanityUrl mustEqual Some(userUpdateRequest.username)
      updatedUser.personalDetails.firstName mustEqual userUpdateRequest.firstName
      updatedUser.personalDetails.lastName mustEqual userUpdateRequest.lastName
      updatedUser.status.receiveGnmMarketing mustEqual userUpdateRequest.receiveGnmMarketing
      updatedUser.status.receive3rdPartyMarketing mustEqual userUpdateRequest.receive3rdPartyMarketing
      updatedUser.status.userEmailValidated mustEqual Some(false)
    }
  }

  "validateEmail" should {
    "set user email validated status to true" in {
      val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])
      val writeRepo = Play.current.injector.instanceOf(classOf[UsersWriteRepository])
      val user1 = createUser()
      val createdUser1 = writeRepo.createUser(user1)

      val result  = writeRepo.validateEmail(User.fromPersistedUser(user1))
      result.isRight mustBe true

      val updatedUser = Await.result(repo.findById(createdUser1.get), 1.second).get
      updatedUser.status.userEmailValidated mustEqual Some(true)
    }
  }

  "delete" should {
    "return true when successful" in {
      val repo = Play.current.injector.instanceOf(classOf[UsersReadRepository])
      val writeRepo = Play.current.injector.instanceOf(classOf[UsersWriteRepository])
      val user1 = createUser()
      val createdUser1 = writeRepo.createUser(user1)
      val origUser = User.fromPersistedUser(user1.copy(_id = createdUser1))

      val result  = writeRepo.delete(origUser)
      result.isRight mustBe true
      Await.result(repo.findById(origUser.id), 1.second) mustEqual None
    }
  }

}
