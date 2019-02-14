package models.database.postgres

import java.util.concurrent.Executors

import actors.metrics.{MetricsActorProvider, MetricsSupport}
import com.google.inject.{Inject, Singleton}
import com.typesafe.scalalogging.LazyLogging
import models.client.{ApiError, ApiResponse}
import scalaz.EitherT
import scalikejdbc._
import scalaz._
import Scalaz._
import models.database.rows.NewsletterSubscriptionRow
import models.database.rows.NewsletterSubscriptionRow.newsletterSubscriptionRowWrites
import net.liftweb.json.JsonDSL._
import net.liftweb.json.{JObject, compactRender}
import play.api.libs.json.Json
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

@Singleton
class PostgresSubjectAccessRequestRepository @Inject()(val metricsActorProvider: MetricsActorProvider) extends LazyLogging
  with PostgresJsonFormats
  with PostgresUtils
  with MetricsSupport {

  implicit val ec : ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def subjectAccessRequestById(id: String): ApiResponse[List[String]] = withMetricsFE("user.subjectAccessRequestById", id) {
    (for {
      accessTokens <- EitherT(getAccessTokens(id))
      guestRegistrationRequest <- EitherT(getGuestRegistrationsRequests(id))
      passwordHashes <- EitherT(getPasswordHashes(id))
      passwordResetRequests <- EitherT(getPasswordResetRequests(id))
      reservedEmails <- EitherT(getReservedEmails(id))
      syncedPrefs <- EitherT(getSyncedPrefs(id))
      user <- EitherT(getUser(id))
      newsLetterSubscriptions <- EitherT(getNewsletterSubscriptions(id))
    } yield accessTokens ++ guestRegistrationRequest ++ passwordHashes ++ passwordResetRequests ++ reservedEmails ++ syncedPrefs ++ user ++ newsLetterSubscriptions).run
  }

  def executeSql(sql: SQL[Nothing, NoExtractor], table: String) = readOnly { implicit session =>
    sql.map(rs => rs.string(1)).list.apply
  }(logFailure(s"failed to run SAR on $table"))


  def getAccessTokens(id:String) = withMetricsFE("sar.getAccessToken", id) {
    val matcher: JObject = "userId" -> id
    val sql =
      sql"""
           |select jsonb_pretty(jsonb_set(accesstokens.jdoc, '{hash}', '"<omitted>"'::jsonb))
           |from accesstokens
           |WHERE jdoc@>${compactRender(matcher)}::jsonb
             """.stripMargin
    executeSql(sql, "getAccessToken")
  }

  def getGuestRegistrationsRequests(id: String) = withMetricsFE("sar.guestRegistrationAttempts", id) {
    val sql =
      sql"""
           |select jsonb_pretty(jdoc)
           |from guestregistrationrequests
           |WHERE id=${id}
           """.stripMargin
    executeSql(sql, "guestRegistrationAttempts")
  }

  def getPasswordHashes(id: String) = withMetricsFE("sar.passwordHashes", id) {
    val sql =
      sql"""
           |select jsonb_pretty(jsonb_set(passwordhashes.jdoc, '{hash}', '"<omitted>"'::jsonb))
           |from passwordhashes
           |WHERE id=${id}
           """.stripMargin
    executeSql(sql, "passwordHashes")
  }

  def getPasswordResetRequests(id: String) = withMetricsFE("sar.passwordResetRequests", id) {
    val sql =
      sql"""
           |select jsonb_pretty(jdoc)
           |from passwordresetrequests
           |WHERE id=${id}
           """.stripMargin
    executeSql(sql, "passwordResetRequests")
  }

  def getReservedEmails(id: String) = withMetricsFE("sar.reservedEmails") {
    val sql =
      sql"""
           |select jsonb_pretty(jdoc)
           |from reservedemails
           |WHERE id=${id}
           """.stripMargin
    executeSql(sql, "reservedEmails")
  }

  def getSyncedPrefs(id: String) = withMetricsFE("sar.syncedPrefs") {
    val sql =
      sql"""
           |select jsonb_pretty(jdoc)
           |from syncedPrefs
           |WHERE id=${id}
           """.stripMargin
    executeSql(sql, "syncedPrefs")
  }

  def getUser(id: String) = withMetricsFE("sar.getUser") {
    val sql =
      sql"""
           |select jsonb_pretty(jdoc)
           |from users
           |WHERE id=${id}
           """.stripMargin
    executeSql(sql, "findById")
  }

  def getNewsletterSubscriptions(id: String) = withMetricsF("sar.getNewsletterSubscriptions") {
    readOnly { implicit session =>
      sql"select * from newsletter_subscriptions where user_id = $id"
        .map(NewsletterSubscriptionRow.apply)
        .list()()
        .map(row => Json.prettyPrint(Json.toJson(row)))
    }(logFailure(s"failed to get newsletter subscriptions for $id"))
  }
}