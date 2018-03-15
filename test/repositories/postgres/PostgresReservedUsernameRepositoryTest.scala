package repositories.postgres

import java.util.UUID

import actors.MetricsActorProviderStub
import akka.actor.ActorSystem
import com.google.common.util.concurrent.MoreExecutors
import models.client.{ReservedUsername, ReservedUsernameList}
import models.database.postgres.PostgresReservedUsernameRepository
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover, Matchers, WordSpecLike}
import play.api.libs.json.Json._
import scalikejdbc._
import support.PgTestUtils

import scala.concurrent.ExecutionContext
import scalaz.\/-

@DoNotDiscover
class PostgresReservedUsernameRepositoryTest extends WordSpecLike
  with Matchers
  with PgTestUtils
  with ScalaFutures
  with BeforeAndAfterAll {

  val actorSystem = ActorSystem()

  override def afterAll(): Unit = {
    actorSystem.terminate()
  }

  trait TestFixture {
    execSql(sql"truncate table reservedusernames")
    private val executor = ExecutionContext.fromExecutor(MoreExecutors.directExecutor())
    val repo = new PostgresReservedUsernameRepository(actorSystem, MetricsActorProviderStub)(executor)
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
          actualNames should contain theSameElementsAs usernames.map(_.username)
        case _ => fail("failed to read a username list")
      }
    }

    "add a reserved username" in new TestFixture {
      whenReady(repo.loadReservedUsernames) {
        case  \/-(list) => list.reservedUsernames.size shouldBe 25
        case _ => fail()
      }
      whenReady(repo.addReservedUsername("ToastyMcToastface")) {
        case  \/-(list) => list.reservedUsernames.size shouldBe 26
        case _ => fail()
      }
    }
  }

}