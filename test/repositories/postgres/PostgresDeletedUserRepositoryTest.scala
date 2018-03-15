package repositories.postgres

import actors.MetricsActorProviderStub
import akka.actor.ActorSystem
import com.google.common.util.concurrent.MoreExecutors
import models.database.postgres.PostgresDeletedUserRepository
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover, Matchers, WordSpecLike}
import scalikejdbc._
import support.PgTestUtils

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.language.postfixOps
import scalaz.\/-

@DoNotDiscover
class PostgresDeletedUserRepositoryTest extends WordSpecLike
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
    private val executor = ExecutionContext.fromExecutor(MoreExecutors.directExecutor())
    val repo = new PostgresDeletedUserRepository(actorSystem, MetricsActorProviderStub)
    execSql(sql"""
                 | INSERT INTO reservedemails (id, jdoc) values
                 | ('1234', '{"_id": "1234", "email": "foo@example.com", "username": "admin"}'::jsonb)
                 | ON CONFLICT (id) DO UPDATE set jdoc=excluded.jdoc
      """.stripMargin)
  }

  "DeletedUserRepository#search" should {
    "find a user when their id matches the query" in new TestFixture {
      whenReady(repo.search("1234"), timeout(5 seconds)) { result =>
        val \/-(searchResponse) = result
        searchResponse.total shouldBe 1
        searchResponse.results should not be empty
      }
    }

    "find a user when their email matches the query" in new TestFixture {
      whenReady(repo.search("foo@example.com"), timeout(5 seconds)) { result =>
        val \/-(searchResponse) = result
        searchResponse.total shouldBe 1
        searchResponse.results should not be empty
      }
    }

    "find a user when their username matches the query" in new TestFixture {
      whenReady(repo.search("admin"), timeout(5 seconds)) { result =>
        val \/-(searchResponse) = result
        searchResponse.total shouldBe 1
        searchResponse.results should not be empty
      }
    }

    "Return an empty SearchResponse if no user is found" in new TestFixture {
      whenReady(repo.search("this wont exist"), timeout(5 seconds)) { result =>
        val \/-(searchResponse) = result
        searchResponse.total shouldBe 0
        searchResponse.results should be(empty)
      }
    }
  }
  "DeletedUserRepository#remove" should {
    "Delete the reserved email for the passed in id" in new TestFixture {
      whenReady(repo.remove("1234"), timeout(5 seconds)) {
        case \/-(result) => result shouldBe 1
        case _ => fail()
      }
      whenReady(repo.search("1234"), timeout(5 seconds)) {
        case \/-(result) => result.total shouldBe 0
        case _ => fail()
      }
    }
  }
}
