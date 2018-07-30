package models.database.postgres

import java.util.concurrent.Executors

import actors.metrics.{MetricsActorProvider, MetricsSupport}
import com.google.inject.{Inject, Singleton}
import com.typesafe.scalalogging.LazyLogging
import models.client.{ApiResponse, User}
import play.api.libs.json.Json
import scalikejdbc._

import scala.concurrent.ExecutionContext

@Singleton
class PostgresSubjectAccessRequestRepository @Inject()(val metricsActorProvider: MetricsActorProvider) extends LazyLogging
  with PostgresJsonFormats
  with PostgresUtils
  with MetricsSupport {

  implicit val ec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def subjectAccessRequestById(id: String): ApiResponse[Option[Int]] = withMetricsFE("user.subjectAccessRequestById", id) {
    val sql =
      sql"""
           |SELECT id FROM users
           |WHERE id=${id}
           |LIMIT 1
             """.stripMargin
    readOnly { implicit session =>
      val userId = sql.map(rs => Json.parse(rs.string(1)).as[Int]).single.apply
      userId
    }(logFailure("Failed to get users SAR"))
  }

}