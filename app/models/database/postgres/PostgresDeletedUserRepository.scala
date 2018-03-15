package models.database.postgres

import java.util.concurrent.Executors

import actors.metrics.{MetricsActorProvider, MetricsSupport}
import akka.actor.ActorSystem
import com.google.inject.{Inject, Singleton}
import com.gu.identity.util.Logging
import com.typesafe.scalalogging.LazyLogging
import models.client.{ApiResponse, SearchResponse}
import models.database.mongo.{DeletedUser, IdentityUser}
import play.api.libs.json.Json
import scalikejdbc._

import scala.concurrent.ExecutionContext
import scalaz.\/-

@Singleton class PostgresDeletedUserRepository @Inject()(val actorSystem: ActorSystem,
                                                         val metricsActorProvider: MetricsActorProvider)
  extends PostgresJsonFormats with PostgresUtils with MetricsSupport with LazyLogging {

  implicit val ec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def findBy(query: String): ApiResponse[Option[DeletedUser]] = withMetricsFE("deletedUser.findBy", query) {
    readOnly { implicit session =>
      val _query = query.toLowerCase
      val idMatcher = s"""{"_id":"${_query}"}"""
      val usernameMatcher = s"""{"username":"${_query}"}"""
      val emailMatcher = s"""{"email":"${_query}"}"""
      val sqlQuery =
        sql"""
             | SELECT jdoc FROM reservedemails
             | WHERE jdoc@>$idMatcher::jsonb
             | OR jdoc@>$emailMatcher::jsonb
             | OR jdoc@>$usernameMatcher::jsonb
             | order by jdoc->>'username'
       """.stripMargin
      sqlQuery.map(_.string(1)).single.apply.map(
        Json.parse(_).as[DeletedUser]
      )
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
    case \/-(Some(user)) =>
      \/-(SearchResponse.create(1, 0, List(IdentityUser(user.email, user.id))))
    case _ =>
      \/-(SearchResponse.create(0, 0, Nil))
  }
}
