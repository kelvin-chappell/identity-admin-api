package models.database.postgres

import com.google.inject.Inject
import com.gu.identity.util.Logging
import configuration.Config.SearchValidation
import models.client.{ApiResponse, SearchResponse, User}
import models.database.mongo.IdentityUser
import play.api.libs.json.Json
import scalikejdbc._

import scala.concurrent.ExecutionContext

class PostgresUsersReadRepository @Inject()(implicit ec: ExecutionContext) extends Logging
  with PostgresJsonFormats
  with PostgresUtils {

  def search(query: String, limit: Option[Int] = None, offset: Option[Int] = None): ApiResponse[SearchResponse] = {
    val _offset = offset.getOrElse(0)
    val _limit = limit.getOrElse(SearchValidation.maximumLimit)
    val lowcaseQuery = query.toLowerCase
    val sql =
      sql"""
       |SELECT jdoc, count(*) OVER() AS full_count FROM users
       |WHERE jdoc #>> '{searchFields,emailAddress}' = ${lowcaseQuery}
       |OR jdoc #>> '{searchFields,username}' = ${lowcaseQuery}
       |OR jdoc #>> '{searchFields,postcode}' = ${lowcaseQuery}
       |OR jdoc #>> '{searchFields,postcodePrefix}' = ${lowcaseQuery}
       |OR jdoc #>> '{searchFields,displayName}' = ${lowcaseQuery}
       |OR jdoc #>> '{privateFields,registrationIp}' = ${lowcaseQuery}
       |OR jdoc #>> '{privateFields,lastActiveIpAddress}' = ${lowcaseQuery}
       |LIMIT ${_limit}
       |OFFSET ${_offset}
       """.stripMargin
    readOnly { implicit session =>
      val results = sql.map(rs => Json.parse(rs.string(1)).as[IdentityUser] -> rs.int(2)).list().apply()
      val count = results.headOption.map(_._2).getOrElse(0)
      SearchResponse.create(count, _offset, results.map(_._1))
    }(logFailure("Failed to search users table"))
  }

  def find(query: String): ApiResponse[Option[User]] = {
    val lowcaseQuery = query.toLowerCase
    val sql =
      sql"""
           |SELECT jdoc FROM users
           |WHERE jdoc #>> '{searchFields,emailAddress}' = ${lowcaseQuery}
           |OR jdoc #>> '{searchFields,username}' = ${lowcaseQuery}
           |OR jdoc #>> '{searchFields,postcode}' = ${lowcaseQuery}
           |OR jdoc #>> '{searchFields,postcodePrefix}' = ${lowcaseQuery}
           |OR jdoc #>> '{searchFields,displayName}' = ${lowcaseQuery}
           |OR jdoc #>> '{privateFields,registrationIp}' = ${lowcaseQuery}
           |OR jdoc #>> '{privateFields,lastActiveIpAddress}' = ${lowcaseQuery}
           |LIMIT 1
             """.stripMargin
    readOnly { implicit session =>
      val user = sql.map(rs => Json.parse(rs.string(1)).as[IdentityUser]).single.apply
      user.map(User.apply)
    }(logFailure("Failed to search users table"))
  }
}
