package models.database.postgres

import com.google.inject.{Inject, Singleton}
import com.gu.identity.util.Logging
import models.client.{ApiResponse, SearchResponse}
import models.database.mongo.{DeletedUser, IdentityUser}
import play.api.libs.json.Json
import scalikejdbc._

import scala.concurrent.ExecutionContext
import scalaz.\/-

@Singleton class PostgresDeletedUserRepository @Inject()(implicit ec: ExecutionContext) extends Logging
  with PostgresJsonFormats
  with PostgresUtils {

  def findBy(query: String): ApiResponse[Option[DeletedUser]] = readOnly { implicit session =>
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
       """.stripMargin
    sqlQuery.map(_.string(1)).single.apply.map(
      Json.parse(_).as[DeletedUser]
    )
  }(logFailure(s"Failed to find deleted users for query: $query"))

  def search(query: String): ApiResponse[SearchResponse] = findBy(query).map {
    case \/-(Some(user)) =>
      \/-(SearchResponse.create(1, 0, List(IdentityUser(user.email, user.id))))
    case _ =>
      \/-(SearchResponse.create(0, 0, Nil))
  }
}
