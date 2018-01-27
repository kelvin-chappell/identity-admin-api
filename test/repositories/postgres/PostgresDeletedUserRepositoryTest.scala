package repositories.postgres

import com.google.common.util.concurrent.MoreExecutors
import models.database.postgres.PostgresDeletedUserRepository
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpecLike}
import scalikejdbc._
import support.EmbeddedPostgresSupport

import scala.concurrent.ExecutionContext
import scalaz.\/-

class PostgresDeletedUserRepositoryTest extends WordSpecLike
  with Matchers
  with EmbeddedPostgresSupport
  with ScalaFutures {

  trait TestFixture {
    private val executor = ExecutionContext.fromExecutor(MoreExecutors.directExecutor())
    val repo = new PostgresDeletedUserRepository()(executor)
    execSql(sql"""
                 | INSERT INTO reservedemails (id, jdoc) values
                 | ('1234', '{"_id": "1234", "email": "foo@example.com", "username": "admin"}'::jsonb)
                 | ON CONFLICT (id) DO UPDATE set jdoc=excluded.jdoc
      """.stripMargin)
  }

  "DeletedUserRepository#search" should {
    "find a user when their id matches the query" in new TestFixture {
      whenReady(repo.search("1234")) { result =>
        val \/-(searchResponse) = result
        searchResponse.total shouldBe 1
        searchResponse.results should not be empty
      }
    }

    "find a user when their email matches the query" in new TestFixture {
      whenReady(repo.search("foo@example.com")) { result =>
        val \/-(searchResponse) = result
        searchResponse.total shouldBe 1
        searchResponse.results should not be empty
      }
    }

    "find a user when their username matches the query" in new TestFixture {
      whenReady(repo.search("admin")) { result =>
        val \/-(searchResponse) = result
        searchResponse.total shouldBe 1
        searchResponse.results should not be empty
      }
    }

    "Return an empty SearchResponse if no user is found" in new TestFixture {
      whenReady(repo.search("this wont exist")) { result =>
        val \/-(searchResponse) = result
        searchResponse.total shouldBe 0
        searchResponse.results should be(empty)
      }
    }
  }

}
