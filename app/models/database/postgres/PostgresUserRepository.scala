package models.database.postgres

import java.util.concurrent.Executors

import actors.metrics.{MetricsActorProvider, MetricsSupport}
import com.google.inject.{Inject, Singleton}
import com.typesafe.scalalogging.LazyLogging
import configuration.Config.SearchValidation
import models.client._
import models.database.mongo.IdentityUser
import play.api.libs.json.{Json, Writes}
import scalikejdbc._

import scala.concurrent.{ExecutionContext, Future}
import scalaz.syntax.either._

@Singleton
class PostgresUserRepository @Inject()(val metricsActorProvider: MetricsActorProvider) extends LazyLogging
  with PostgresJsonFormats
  with PostgresUtils
  with MetricsSupport {

  implicit val ec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  private def createGinSearchJson(fieldName: String, innerFieldName: String, value: String) =
      Json.toJson(Map(fieldName -> Map(innerFieldName -> value.toLowerCase)))(Writes.mapWrites).toString // Writes.mapWrites has to be explicit because of MongoJsonFormats.objectMapFormat

  def search(queryValue: String, limit: Option[Int] = None, offset: Option[Int] = None): ApiResponse[SearchResponse] = withMetricsFE("user.search", s"$queryValue $limit $offset") {


    val _offset = offset.getOrElse(0)
    val _limit = limit.getOrElse(SearchValidation.maximumLimit)
    val sql =
      sql"""|SELECT jdoc, count(*) OVER() AS full_count FROM users
            |WHERE id = ${queryValue.toLowerCase}
            |OR jdoc@>${createGinSearchJson("searchFields", "emailAddress", queryValue)}::jsonb
            |OR jdoc@>${createGinSearchJson("searchFields", "username", queryValue)}::jsonb
            |OR jdoc@>${createGinSearchJson("searchFields", "postcode", queryValue)}::jsonb
            |OR jdoc@>${createGinSearchJson("searchFields", "postcodePrefix", queryValue)}::jsonb
            |OR jdoc@>${createGinSearchJson("searchFields", "displayName", queryValue)}::jsonb
            |OR jdoc@>${createGinSearchJson("privateFields", "registrationIp", queryValue)}::jsonb
            |OR jdoc@>${createGinSearchJson("privateFields", "lastActiveIpAddress", queryValue)}::jsonb
            |order by jdoc#>'{primaryEmailAddress}'
            |LIMIT ${_limit}
            |OFFSET ${_offset}
            |""".stripMargin
    readOnly { implicit session =>
      val results = sql.map(rs => Json.parse(rs.string(1)).as[IdentityUser] -> rs.int(2)).list().apply()
      val count = results.headOption.map(_._2).getOrElse(0)
      SearchResponse.create(count, _offset, results.map(_._1))
    }(logFailure("Failed to search users table"))
  }

  def findById(id: String): ApiResponse[Option[User]] = withMetricsFE("user.findById", id) {
    val sql =
      sql"""
           |SELECT jdoc FROM users
           |WHERE id=${id}
           |LIMIT 1
             """.stripMargin
    readOnly { implicit session =>
      val user = sql.map(rs => Json.parse(rs.string(1)).as[IdentityUser]).single.apply
      user.map(User.apply)
    }(logFailure("Failed find user by id"))
  }

  def findByEmail(emailAddress: String): ApiResponse[Option[User]] = withMetricsFE("user.findByEmail", emailAddress) {
    val sql =
      sql"""
           |SELECT jdoc FROM users
           |WHERE jdoc@>${createGinSearchJson("searchFields", "emailAddress", emailAddress)}::jsonb
           |order by jdoc#>'{primaryEmailAddress}'
           |LIMIT 1
           |""".stripMargin
    readOnly { implicit session =>
      val user = sql.map(rs => Json.parse(rs.string(1)).as[IdentityUser]).single.apply
      user.map(User.apply)
    }(logFailure("Failed find user by emailAddress"))
  }

  def update(user: User, userUpdateRequest: UserUpdateRequest): ApiResponse[User] = withMetricsFE("user.update", s"$user $userUpdateRequest") {
    val persistedUser = DB.readOnly { implicit s =>
      sql"select jdoc from users where id=${user.id} limit 1"
        .map(rs => Json.parse(rs.string(1)).as[IdentityUser])
        .single().apply()
    }
    persistedUser.map { originalUser =>
      val update = originalUser.mergeWith(userUpdateRequest)
      val updateJson = Json.toJson(update).toString()
      localTx { implicit s =>
        sql"""update users set jdoc=${updateJson}::jsonb where id=${originalUser._id}""".update().apply()
        User(update)
      }(logFailure(s"Failed to update user: ${user.id}"))
    }.getOrElse(Future.successful(
      ApiError("Not Found", "The user was not found").left[User]
    )).recover { case error =>
      val title = s"Failed to update user ${user.id}"
      logger.error(title, error)
      ApiError(title, error.getMessage).left[User]
    }
  }
  
  def updateEmailValidationStatus(user: User, emailValidated: Boolean): ApiResponse[User] = withMetricsFE("user.updateEmailValidationStatus", s"$user $emailValidated") {
    val boolString = emailValidated.toString.toLowerCase
    val query = sql"update users set jdoc=jsonb_set(jdoc, '{statusFields,userEmailValidated}', ${boolString}::jsonb) where id=${user.id} "
    localTx { implicit s =>
      query.update().apply()
      user
    }(logFailure(s"Failed to update email validated status for user ${user.id}"))
  }

  def unsubscribeFromMarketingEmails(email: String): ApiResponse[Int] = withMetricsFE("user.unsubscribeFromMarketingEmails", email) {
    val query = sql"update users set jdoc=jsonb_set(jdoc, '{statusFields,receive3rdPartyMarketing}', 'false'::jsonb) where (jdoc#>>'{searchFields,emailAddress}')=${email} "
    localTx { implicit s =>
      query.update().apply()
    }(logFailure(s"Failed to update email validated status for user $email"))
  }

}
