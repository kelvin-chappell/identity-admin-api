package actors

import actors.metrics.MetricsActorProvider
import akka.actor.{ActorRef, ActorSystem}
import org.scalatest.mockito.MockitoSugar

object TestActorSystem extends MockitoSugar {
  val testActorSystem = ActorSystem()
}

object MetricsActorProviderStub extends MetricsActorProvider(TestActorSystem.testActorSystem, null) with MockitoSugar {
  override def get(): ActorRef = mock[ActorRef]
}
