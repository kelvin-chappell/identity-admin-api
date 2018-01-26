package repositories.postgres

import java.util.UUID

import com.google.common.util.concurrent.MoreExecutors
import models.client.{ReservedUsername, ReservedUsernameList}
import models.database.postgres.PostgresReservedUsernameRepository
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpecLike}
import play.api.libs.json.Json._
import scalikejdbc._
import support.EmbeddedPostgresSupport

import scala.concurrent.ExecutionContext
import scalaz.\/-

class PostgresReservedUsernameRepositoryTest extends WordSpecLike
  with Matchers
  with EmbeddedPostgresSupport
  with ScalaFutures {

  trait TestFixture {
    private val executor = ExecutionContext.fromExecutor(MoreExecutors.directExecutor())
    val repo = new PostgresReservedUsernameRepository()(executor)
    val usernames = List.fill(25)(ReservedUsername(UUID.randomUUID().toString))
    usernames.foreach { username =>
      execSql(
        sql"""
             | INSERT INTO reservedusernames (id, jdoc) values
             | (${username.username}, ${stringify(toJson(username))}::jsonb)
             | ON CONFLICT (id) DO UPDATE set jdoc=excluded.jdoc
        """.stripMargin)

    }
  }

  "PostgresReservedUsernameRepository#loadReservedUsernames" should {
    "load all reserved usernames" in new TestFixture {
      whenReady(repo.loadReservedUsernames) {
        case \/-(ReservedUsernameList(actualNames)) =>
          actualNames should contain theSameElementsAs(usernames.map(_.username))
        case _ => fail("failed to read a username list")
      }
    }
  }

}