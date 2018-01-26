package monitoring

import akka.actor.ActorSystem
import models.database.mongo.UsersReadRepository
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.concurrent.Eventually
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Environment

import scala.concurrent.{ExecutionContext, Future}

class MongoDBHealthCheckTest extends FunSuite with MockitoSugar with Eventually with Matchers with GuiceOneServerPerSuite {

  implicit val ec = app.injector.instanceOf[ExecutionContext]

  val actorSystem = ActorSystem("test")

  val userRepo = mock[UsersReadRepository]
  val metrics = mock[Metrics]
  val env = mock[Environment]

  implicit override val patienceConfig =
    PatienceConfig(timeout = scaled(Span(2, Seconds)), interval = scaled(Span(5, Millis)))

  test("report as healthy when mongo query is successful") {
    val hc = new MongoDBHealthCheck(env, userRepo, actorSystem, metrics)
    when(userRepo.count()).thenReturn(Future.successful(10))

    hc.triggerUpdate()
    eventually {
      verify(metrics).publishCount(hc.MetricName, 1)
    }
  }

  test("report as unhealthy when mongo query returns zero users") {
    val userRepo = mock[UsersReadRepository]
    val metrics = mock[Metrics]
    val hc = new MongoDBHealthCheck(env, userRepo, actorSystem, metrics)
    when(userRepo.count()).thenReturn(Future.successful(0))

    hc.triggerUpdate()
    eventually {
      verify(metrics).publishCount(hc.MetricName, 0)
    }
  }

  test("report as unhealthy when mongo query fails") {
    val hc = new MongoDBHealthCheck(env, userRepo, actorSystem, metrics)
    when(userRepo.count()).thenReturn(Future.failed(new IllegalStateException("boom")))

    hc.triggerUpdate()
    eventually {
      verify(metrics).publishCount(hc.MetricName, 0)
    }
  }

}
