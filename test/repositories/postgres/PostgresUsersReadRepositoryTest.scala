package repositories.postgres

import com.google.common.util.concurrent.MoreExecutors
import models.client.{ApiResponse, SearchResponse}
import models.database.mongo.{IdentityUser, PrivateFields, SearchFields, UserDates}
import models.database.postgres.PostgresUsersReadRepository
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpecLike}
import play.api.libs.json.Json
import scalikejdbc._
import support.EmbeddedPostgresSupport

import scala.concurrent.ExecutionContext
import scalaz.\/-
import scalaz.syntax.std.option._

class PostgresUsersReadRepositoryTest extends WordSpecLike
  with Matchers
  with EmbeddedPostgresSupport
  with ScalaFutures {

  trait TestFixture {
    private val executor = ExecutionContext.fromExecutor(MoreExecutors.directExecutor())
    val repo = new PostgresUsersReadRepository()(executor)
    val testUser = IdentityUser(
      "identitydev@guardian.co.uk", "1000001",
      searchFields = SearchFields(
        "identitydev@guardian.co.uk".some, "username".some, "displayname".some, "n19ag".some, "n1".some
      ).some, publicFields = None,
      privateFields = PrivateFields(
        registrationIp = "1.2.3.4".some, lastActiveIpAddress = "4.5.6.7".some
      ).some,
      dates = UserDates(
        lastActivityDate = new DateTime(42000l, DateTimeZone.UTC).some
      ).some
    )
    val userJson = Json.stringify(Json.toJson(testUser))
    execSql(
      sql"""
         | INSERT INTO users (id, jdoc) values
         | ('1234', ${userJson}::jsonb)
         | ON CONFLICT (id) DO UPDATE set jdoc=excluded.jdoc
      """.stripMargin)
  }

  "UserReadRepository#search" should {
    def checkOneResult(f: => ApiResponse[SearchResponse]) = whenReady(f) { result =>
      val \/-(searchResponse) = result
      searchResponse.total shouldBe 1
      searchResponse.results should not be empty
    }

    "find a user when their email address matches the query" in new TestFixture {
      checkOneResult(repo.search("identitydev@guardian.co.uk"))
    }

    "find a user when their username matches the query" in new TestFixture {
      checkOneResult(repo.search("username"))
    }

    "find a user when their postcode matches the query" in new TestFixture {
      checkOneResult(repo.search("N19AG"))
    }

    "find a user when their postcode prefix matches the query" in new TestFixture {
      checkOneResult(repo.search("N1"))
    }

    "find a user when their displayName matches the query" in new TestFixture {
      checkOneResult(repo.search("displayname"))
    }

    "find a user when their registrationIp matches the query" in new TestFixture {
      checkOneResult(repo.search("1.2.3.4"))
    }

    "find a user when their lastActiveIp matches the query" in new TestFixture {
      checkOneResult(repo.search("4.5.6.7"))
    }

    "find multiple users if they match" in new TestFixture {
      val secondUser = Json.stringify(Json.toJson(testUser.copy(primaryEmailAddress = "foo@bar.com", _id = "10000123")))
      execSql(sql"""
               | INSERT INTO users (id, jdoc) values
               | ('12345', ${secondUser}::jsonb)
               | ON CONFLICT (id) DO UPDATE set jdoc=excluded.jdoc
             """.stripMargin)
      whenReady(repo.search("username")) { result =>
        val \/-(searchResponse) = result
        searchResponse.total shouldBe 2
      }
    }
  }

  "UserReadRepository#find" should {
    "Find a single user" in new TestFixture {
      whenReady(repo.find("identitydev@guardian.co.uk")) {
        case \/-(maybeUser) => maybeUser should not be empty
        case _ => fail("expected to find a user")
      }
    }
    "Read ISO-8601 formatted date time strings to DateTime objects" in new TestFixture {
      whenReady(repo.find("identitydev@guardian.co.uk")) {
        case \/-(Some(user)) =>
          user.lastActivityDate shouldBe new DateTime(42000l, DateTimeZone.UTC).some
        case _ => fail("expected to find a user")
      }
    }
  }

}