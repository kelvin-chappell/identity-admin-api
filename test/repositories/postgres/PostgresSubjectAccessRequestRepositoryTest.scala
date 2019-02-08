package repositories.postgres

import actors.MetricsActorProviderStub
import akka.actor.ActorSystem
import models.database.mongo._
import models.database.postgres.PostgresSubjectAccessRequestRepository
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover, Matchers, WordSpecLike}
import org.scalatest.concurrent.ScalaFutures
import support.PgTestUtils
import scalikejdbc._
import play.api.libs.json.Json
import scalaz.\/-
import scalaz.syntax.std.option._

@DoNotDiscover
class PostgresSubjectAccessRequestRepositoryTest extends WordSpecLike
  with Matchers
  with PgTestUtils
  with ScalaFutures
  with BeforeAndAfterAll {

  val actorSystem = ActorSystem()

  override def afterAll(): Unit = {
    super.afterAll()
    actorSystem.terminate()
  }

  trait TestFixture {

    val repo = new PostgresSubjectAccessRequestRepository(MetricsActorProviderStub)
    implicit val ec = repo.ec
    execSql(sql"delete from newsletter_subscriptions")
    execSql(sql"delete from users")
    execSql(sql"delete from accesstokens")
    execSql(sql"delete from autosignintokens")

    val testUser = IdentityUser(
      "identitydev@guardian.co.uk", "1234",
      searchFields = SearchFields(
        "identitydev@guardian.co.uk".some, "username".some, "displayname".some, "n19ag".some, "n1".some
      ).some, publicFields = None,
      privateFields = PrivateFields(
        registrationIp = "1.2.3.4".some, lastActiveIpAddress = "4.5.6.7".some
      ).some,
      dates = UserDates(
        lastActivityDate = new DateTime(42000l, DateTimeZone.UTC).some
      ).some,
      statusFields = StatusFields().some
    )
    val userJson = Json.stringify(Json.toJson(testUser))

    execSql(
      sql"""
           | INSERT INTO users (id, jdoc) values
           | ('1234', ${userJson}::jsonb)
           | ON CONFLICT (id) DO UPDATE set jdoc=excluded.jdoc
      """.stripMargin)

    execSql(
      sql"""
           |INSERT INTO accesstokens
           | SELECT tk.new_token->>'hash', tk.new_token
           | FROM (SELECT jsonb_build_object(
           |  'hash', md5(random()::text || clock_timestamp()::text),
           |  'active', true,
           |  'expiryTime', '3000-01-01T00:00:00.000Z',
           |  'client', 'CHANGE_ME',
           |   'isClientToken', true,
           |  'userId', '1234',
           |  'scopes', jsonb_build_array('Some_Scope', 'Some_Other_Scope')
           |) as new_token) as tk;
         """.stripMargin)
  }

  "PostgresSubjectAccessRequestRepository" should {

    "get a users string" in new TestFixture {
      whenReady(repo.getUser("1234")) { result =>
        result.map(userStrings => Json.parse(userStrings.head).as[IdentityUser]._id shouldEqual "1234")
      }
    }

    "return empty list if user is not in DB" in new TestFixture {
      whenReady(repo.getUser("1111")) { result =>
        result.map(userStrings => userStrings shouldEqual List.empty)
      }
    }

    "get a users access tokens" in new TestFixture {

      whenReady(repo.getAccessTokens("1234")) { result =>
        result.map( _.head should include ("3000-01-01T00:00:00.000Z"))
      }
    }

    "subjectAccessRequestById gets a list of users data from tables" in new TestFixture {
      whenReady(repo.subjectAccessRequestById("1234")) { result =>
        result.map(_.length shouldEqual(2))
      }
    }

    "getNewsletterSubscriptions" in new TestFixture {
      execSql(sql"INSERT INTO users values ('12345', '{}')")
      execSql(sql"INSERT INTO newsletter_subscriptions values ('1234', 'uk-today', true, '2017-01-01')")
      execSql(sql"INSERT INTO newsletter_subscriptions values ('12345', 'au-today', true, '2017-01-01')")
      execSql(sql"INSERT INTO newsletter_subscriptions values ('1234', 'us-today', false, '2017-01-01')")

      whenReady(repo.getNewsletterSubscriptions("1234")) {_ shouldBe \/-(List(
        """|{
           |  "userId" : "1234",
           |  "newsLetterName" : "uk-today",
           |  "subscribed" : true,
           |  "lastUpdated" : "2017-01-01 00:00:00.0"
           |}""".stripMargin,
        """|{
           |  "userId" : "1234",
           |  "newsLetterName" : "us-today",
           |  "subscribed" : false,
           |  "lastUpdated" : "2017-01-01 00:00:00.0"
           |}""".stripMargin
      ))}
    }

    "getAutoSignInTokens" in new TestFixture {
      execSql(sql"INSERT INTO users values ('54321', '{}')")
      execSql(
        sql"""
             | INSERT INTO autosignintokens VALUES ('123-token', '54321', '1234@email.com', '3000-01-01T00:00:00.000Z', false, false)
         """.stripMargin)

      whenReady(repo.getAutoSignInTokens("54321")) {_ shouldBe \/-(List(
        """|{
           |  "identityId" : "54321",
           |  "emailAddress" : "1234@email.com",
           |  "created" : "3000-01-01 00:00:00.0",
           |  "hasBeenUsed" : false,
           |  "hasBeenInvalidated" : false
           |}""".stripMargin
      ))}
    }
  }
}
