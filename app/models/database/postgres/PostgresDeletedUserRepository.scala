package models.database.postgres

import java.util.concurrent.Executors

import actors.metrics.{MetricsActorProvider, MetricsSupport}
import akka.actor.ActorSystem
import com.google.inject.{Inject, Singleton}
import com.typesafe.scalalogging.LazyLogging
import models.client.{ApiResponse, SearchResponse}
import models.database.mongo.{DeletedUser, IdentityUser, PublicFields}
import play.api.libs.json.{JsObject, JsString, Json}
import scalikejdbc._

import scala.concurrent.ExecutionContext
import scalaz.{-\/, \/-}
import scalaz.syntax.std.option._

@Singleton class PostgresDeletedUserRepository @Inject()(val actorSystem: ActorSystem,
                                                         val metricsActorProvider: MetricsActorProvider)
  extends PostgresJsonFormats with PostgresUtils with MetricsSupport with LazyLogging {

  implicit val ec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def findBy(query: String): ApiResponse[List[DeletedUser]] = withMetricsFE("deletedUser.findBy", query) {
    readOnly { implicit session =>
      val _query = query.toLowerCase.trim
      val usernameMatcher = JsObject(Seq("username" -> JsString(_query))).toString
      val emailMatcher = JsObject(Seq("email" -> JsString(_query))).toString
      val sqlQuery =
        sql"""
             | SELECT jdoc FROM reservedemails
             | WHERE id=${_query}
             | OR jdoc@>$emailMatcher::jsonb
             | OR jdoc@>$usernameMatcher::jsonb
             | order by jdoc->>'username'
       """.stripMargin
      sqlQuery.map(_.string(1)).list().apply.map(Json.parse(_).as[DeletedUser])
    }(logFailure(s"Failed to find deleted users for query: $query"))
  }

  def remove(id: String): ApiResponse[Int] = withMetricsFE("deletedUser.remove", id) {
    val sqlQuery =
      sql"""
           | DELETE FROM reservedemails
           | WHERE id=${id}
       """.stripMargin
    localTx { implicit session =>
      sqlQuery.update().apply()
    }(logFailure(s"Failed to unreserve email for id $id"))
  }

  def search(query: String): ApiResponse[SearchResponse] = findBy(query).map {
    case \/-(results) =>
      val _results = results.map { u =>
        val username = if (u.username.nonEmpty) u.username.some else None
        IdentityUser(u.email, u.id, publicFields = PublicFields(username = username).some)
      }
      \/-(SearchResponse.create(_results.size, 0, _results))
    case -\/(error) =>
      logger.error("Failed to search for deleted users", error)
      \/-(SearchResponse.create(0, 0, Nil))
  }

  def setBlocklisted(reservationId: String): ApiResponse[Int] = {
    val sql =
      sql"""
        | UPDATE reservedemails SET permanent=TRUE WHERE id=$reservationId
      """.stripMargin
    localTx { implicit session =>
      sql.update().apply()
    }(logFailure(s"Failed to permanently reserve $reservationId"))
  }

  def isBlocklisted(reservationId: String): ApiResponse[Boolean] = {
    val sql =
      sql"""
           | select count(*) from reservedemails WHERE id=$reservationId AND permanent=TRUE
      """.stripMargin
    localTx { implicit session =>
      sql.map(_.int(1)).single().apply().getOrElse(0) > 0
    }(logFailure(s"Failed to check reservation status $reservationId"))
  }
}
