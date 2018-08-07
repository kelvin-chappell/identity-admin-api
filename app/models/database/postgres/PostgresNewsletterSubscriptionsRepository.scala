package models.database.postgres

import java.sql.Timestamp
import java.time.Instant
import java.util.concurrent.Executors

import actors.metrics.{MetricsActorProvider, MetricsSupport}
import com.google.inject.{Inject, Singleton}
import com.typesafe.scalalogging.LazyLogging
import models.client.ApiResponse
import scalikejdbc._

import scala.concurrent.ExecutionContext

@Singleton
class PostgresNewsletterSubscriptionsRepository @Inject()(val metricsActorProvider: MetricsActorProvider) extends PostgresJsonFormats with PostgresUtils with LazyLogging with MetricsSupport {

  implicit val ec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def unsubscribeAll(userId: String): ApiResponse[Int] = withMetricsFE("pgnewsletters.unsubscribeAll") {
    val timestamp = new Timestamp(Instant.now.toEpochMilli)
    localTx { implicit s =>
      sql"UPDATE newsletter_subscriptions SET subscribed = FALSE, last_updated = $timestamp WHERE user_id = $userId".update()()
    }(logFailure(s"Failed to unsubscribeAll for user $userId"))
  }

}
