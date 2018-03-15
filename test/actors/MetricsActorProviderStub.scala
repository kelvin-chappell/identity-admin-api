package actors

import actors.metrics.MetricsActorProvider
import akka.actor.ActorRef
import org.scalatest.mockito.MockitoSugar

object MetricsActorProviderStub extends MetricsActorProvider(null, null) with MockitoSugar {
  override def get(): ActorRef = mock[ActorRef]
}
