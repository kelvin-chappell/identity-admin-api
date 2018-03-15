package models.database.postgres

import java.util.UUID

import actors.metrics.{MetricsActorProvider, MetricsSupport}
import akka.actor.ActorSystem
import com.google.inject.Inject
import com.typesafe.scalalogging.LazyLogging
import models.client
import models.client.{ApiError, ApiResponse, ReservedUsername, ReservedUsernameList}
import play.api.libs.json.Json.{parse, stringify, toJson}
import scalikejdbc._

import scala.concurrent.{ExecutionContext, Future}
import scalaz.{-\/, \/-}

class PostgresReservedUsernameRepository @Inject()(val actorSystem: ActorSystem,
                                                   val metricsActorProvider: MetricsActorProvider)(implicit ec: ExecutionContext) extends LazyLogging
  with PostgresJsonFormats
  with PostgresUtils
  with MetricsSupport {

  def loadReservedUsernames: ApiResponse[ReservedUsernameList] = withMetricsFE("reserved.loadReservedUsernames") {
    readOnly { implicit session =>
      val sql = sql"SELECT jdoc from reservedusernames order by id"
      val results = sql.map(rs => parse(rs.string(1)).as[ReservedUsername]).list().apply()
      client.ReservedUsernameList(results.map(_.username))
    }(logFailure("Failed to load reserved usernames"))
  }

  def addReservedUsername(username: String): ApiResponse[ReservedUsernameList] = withMetricsFE("reserved.addReservedUsername", username) {
    val randomId = UUID.randomUUID().toString
    val record = stringify(toJson(ReservedUsername(username)))
    val sql = sql"INSERT INTO reservedusernames (id, jdoc) values ($randomId, ${record}::jsonb)"
    localTx { implicit session =>
      sql.update().apply()
    }(logFailure(s"Failed to reserve username $username")) flatMap {
      case \/-(_) => loadReservedUsernames
      case _ => Future.successful(-\/(ApiError(s"Failed to add $username to reserved username list")))
    }
  }
}
