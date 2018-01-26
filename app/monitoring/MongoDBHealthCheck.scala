package monitoring

import akka.actor.ActorSystem
import com.gu.identity.util.Logging

import scala.concurrent.duration._
import scala.util.{Failure, Success}
import javax.inject._

import configuration.Config
import models.database.mongo.UsersReadRepository
import play.api.Environment

import scala.concurrent.ExecutionContext

class MongoDBHealthCheck @Inject() (
    environment: Environment,
    userRepository: UsersReadRepository,
    actorSystem: ActorSystem,
    metrics: Metrics)(implicit ec: ExecutionContext) extends Logging {
  private[monitoring] val MetricName = "MongoConnectivity"

  private[monitoring] def triggerUpdate() {
    logger.debug("Updating mongo connectivity health check")

    userRepository.count().onComplete {
      case Success(count) =>
        if(count > 0)
          metrics.publishCount(MetricName, 1)
        else {
          logger.error("Mongo DB is reporting zero users")
          metrics.publishCount(MetricName, 0)
        }
      case Failure(t) =>
        logger.error("Error connecting to mongo", t)
        metrics.publishCount(MetricName, 0)
    }
  }

  def start() {
    if(Config.Monitoring.mongoEnabled) {
      // trigger immediately
      triggerUpdate()

      // trigger every 10 seconds with an initial delay of 30 seconds
      actorSystem.scheduler.schedule(30.seconds, 10.seconds) {
        triggerUpdate()
      }
    }
  }

  start()
}
