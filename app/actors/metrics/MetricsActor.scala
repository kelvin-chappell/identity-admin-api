package actors.metrics

import javax.inject.{Inject, Provider, Singleton}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import monitoring.{CloudWatch, Sample, Samples}

import scala.concurrent.duration._
import scala.language.postfixOps

class MetricsActor(cloudWatch: CloudWatch) extends Actor {
  import MetricsActor._
  context.system.scheduler.schedule(10 seconds, 10 seconds, self, Tick)(context.dispatcher)

  var samples = Samples()

  def receive = {
    case Tick =>
      cloudWatch.publishMetrics(samples)
      samples = Samples()
    case s: Sample =>
      samples = samples + s
  }
}

object MetricsActor {
  private case object Tick
  def props(cloudWatch: CloudWatch): Props = Props(new MetricsActor(cloudWatch))
}


@Singleton
class MetricsActorProvider @Inject()(val actorSystem: ActorSystem, cloudwatch: CloudWatch) extends Provider[ActorRef] {
  private lazy val metricsActorProvider: ActorRef = actorSystem.actorOf(MetricsActor.props(cloudwatch))

  override def get(): ActorRef = metricsActorProvider
}
