package util.scientist

import ai.x.diff._
import cats.instances.all._
import cats.instances.future
import cats.{Id, Monad}
import com.google.common.util.concurrent.MoreExecutors
import models.client.{ApiError, UserSummary}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.{ExecutionContext, Future}
import scalaz.{\/, \/-}

class ScientistTest extends FlatSpec with Matchers with ScalaFutures with MockitoSugar {

  import Experiment._

  object Box {
    var lastResult: Result = null
    val publish: List[Int] => Result => Unit = list => result => Box.lastResult = result
  }

  case class Test[A, M[_], E](returns: A, expectedResult: Result, experiment: Experiment[A, M, E])

  implicit val experimentSettings = ExperimentSettings[List[Int]](Box.publish)
  implicit val sameThreadExecutor = ExecutionContext.fromExecutor(MoreExecutors.directExecutor())
  implicit val futureMonad: Monad[Future] = future.catsStdInstancesForFuture(sameThreadExecutor)

  "Science" should "run and compare experiments" in {
    val tests = List(
      Test[List[Int], Id, Throwable](
        List(1, 2, 3),
        MisMatch(List(1, 2, 3), List(2, 3, 4)),
        Experiment.sync(
          name = "a",
          control = List(1, 2, 3),
          candidate = List(2, 3, 4)
        )
      )
    )

    tests.foreach { test =>
      val returned = test.experiment.run
      returned shouldBe test.returns
      Box.lastResult shouldBe test.expectedResult
    }
  }

  it should "not run and report disabled experiments" in {
    implicit val experimentSettings = new ExperimentSettings[List[Int]](Box.publish) {
      override def isEnabled(experimentName: String): Boolean = experimentName != "b"
    }

    val tests = List(
      Test[List[Int], Id, Throwable](
        List(1, 2, 3),
        MisMatch(List(1, 2, 3), List(2, 3, 4)),
        Experiment.sync(
          name = "a",
          control = List(1, 2, 3),
          candidate = List(2, 3, 4)
        )
      ),
      Test[List[Int], Id, Throwable](
        List(1, 2, 3),
        DisabledExperiment("b"),
        Experiment.sync(
          name = "b",
          control = List(1, 2, 3),
          candidate = List(2, 3, 4)
        )
      )
    )

    tests.foreach { test =>
      val returned = test.experiment.run
      returned shouldBe test.returns
      Box.lastResult shouldBe test.expectedResult
    }
  }

  it should "support futures when both sides are successful" in {
    val experiment = Experiment.async[List[Int]](
      "async-test",
      Future.successful(List(1,2,3)),
      Future.successful(List(1,2,3))
    )
    whenReady(experiment.run) { result =>
      result.shouldBe(List(1,2,3))
      Box.lastResult shouldBe Match(List(1,2,3), List(1,2,3))
    }
  }

  it should "support futures when one side is failed" in {
    val exception = new IllegalArgumentException("test")
    val experiment = Experiment.async[List[Int]](
      "async-test",
      Future.successful(List(1,2,3)),
      Future.failed(exception)
    )
    whenReady(experiment.run) { result =>
      result.shouldBe(List(1,2,3))
      Box.lastResult shouldBe ExperimentFailure(exception.toString)
    }
  }

  it should "Workd" in {
    implicit val s = implicitly[DiffShow[\/[ApiError, UserSummary]]]
    val experiment = Experiment.async[\/[ApiError, UserSummary]](
      "async-test",
      Future.successful(\/-(UserSummary("1234", "foo@bar.com", orphan = false))),
      Future.successful(\/-(UserSummary("123", "foo@bar.com", orphan = false)))
    ).run

    whenReady(experiment) { foo =>
      Thread.sleep(1000)
    }

  }
}
