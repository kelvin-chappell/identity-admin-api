package actors.metrics

import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorSystem
import akka.pattern.after
import com.typesafe.scalalogging.LazyLogging
import monitoring.{IncrementCounter, Sample}
import util.StopWatch

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import scalaz._
import Scalaz._

object MetricsSupport {
  case class Namespace(name: String)
}

trait MetricsSupport extends LazyLogging {
  import MetricsSupport._

  def actorSystem: ActorSystem = metricsActorProvider.actorSystem
  def metricsActorProvider: MetricsActorProvider
  private implicit def ec = actorSystem.dispatcher

  private def reportSuccess(inProgressSwitch: AtomicReference[Boolean], namespace: Namespace, name: String, stopWatch: StopWatch): Unit = {
    if (inProgressSwitch.getAndSet(false)) {
      metricsActorProvider.get() ! Sample(namespace.name, s"$name-success-latency", stopWatch.elapsed)
      metricsActorProvider.get() ! IncrementCounter(namespace.name, s"$name-success-count")
    }
  }

  private def reportFailure(inProgressSwitch: AtomicReference[Boolean], namespace: Namespace, name: String, stopWatch: StopWatch): Unit = {
    if (inProgressSwitch.getAndSet(false)) {
      val elapsed = stopWatch.elapsed
      logger.warn(s"MetricsSupport reportFailure ${namespace.name} $name $elapsed")
      metricsActorProvider.get() ! Sample(namespace.name, s"$name-failure-latency", elapsed)
      metricsActorProvider.get() ! IncrementCounter(namespace.name, s"$name-failure-count")
    }
  }

  private def longWaitMetric(inProgressSwitch: AtomicReference[Boolean], namespace: Namespace, name: String, stopWatch: StopWatch) = {
    after(30 seconds, actorSystem.scheduler) {
       Future(reportFailure(inProgressSwitch, namespace, name, stopWatch))(actorSystem.dispatcher)
    }(actorSystem.dispatcher)
  }

  /**
    * Records failure if operation takes more than 30 seconds
    */
  def withMetricsF[A](name: String, errorMessage: String = "Error")(block: => Future[A])(implicit namespace: Namespace): Future[A] = {
    val inProgressSwitch = new AtomicReference[Boolean](true)
    val stopWatch = new StopWatch
    longWaitMetric(inProgressSwitch, namespace, name, stopWatch)

    Try(block) match {
      case Success(result) =>
        result.foreach(_ => reportSuccess(inProgressSwitch, namespace, name, stopWatch))
        result.failed.foreach(_ => reportFailure(inProgressSwitch, namespace, name, stopWatch))
        result
      case Failure(e) =>
        logger.error(s"$name $errorMessage", e)
        reportFailure(inProgressSwitch, namespace, name, stopWatch)
        Future.failed(e)
    }
  }

  /**
    * Records failure if operation takes more than 30 seconds
    */
  def withMetricsFE[A,B](name: String, errorMessage: String = "Error")(block: => Future[\/[A, B]])(implicit namespace: Namespace): Future[\/[A, B]] = {
    val inProgressSwitch = new AtomicReference[Boolean](true)
    val stopWatch = new StopWatch
    longWaitMetric(inProgressSwitch, namespace, name, stopWatch)

    Try(EitherT(block)) match {
      case Success(result) =>
        result.map(_ => reportSuccess(inProgressSwitch, namespace, name, stopWatch))
        result.leftMap(_ => reportFailure(inProgressSwitch, namespace, name, stopWatch))
        result.run.failed.foreach(_ => reportFailure(inProgressSwitch, namespace, name, stopWatch))
        result.run
      case Failure(e) =>
        logger.error(s"$name $errorMessage", e)
        reportFailure(inProgressSwitch, namespace, name, stopWatch)
        Future.failed(e)
    }
  }


}