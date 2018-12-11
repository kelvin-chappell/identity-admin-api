package models.database.postgres

import actors.metrics.{MetricsActorProvider, MetricsSupport}
import com.google.inject.Inject
import com.typesafe.scalalogging.LazyLogging
import models.client.ApiResponse
import net.liftweb.json.JsonDSL._
import net.liftweb.json.compactRender
import scalikejdbc._

import scala.concurrent.ExecutionContext

class PostgresReservedEmailRepository @Inject()(val metricsActorProvider: MetricsActorProvider)(implicit ec: ExecutionContext) extends LazyLogging
  with PostgresJsonFormats
  with PostgresUtils
  with MetricsSupport {

  def isReserved(email: String): ApiResponse[Boolean] = {
    val emailMatcher = compactRender("email" -> email)
    val sql =
      sql"""
           | select count(*) from reservedemails where (jdoc@>${emailMatcher}::jsonb)
      """.stripMargin
    readOnly { implicit session =>
      sql.map(_.int(1)).single().apply().getOrElse(0) > 0
    }(logFailure(s"Failed to check if email is reserved"))
  }
}
