package util.scientist

import java.util.concurrent.Executors

import ai.x.diff._
import akka.actor.ActorSystem
import com.typesafe.scalalogging.LazyLogging
import akka.pattern.after
import models.client.User
import org.joda.time.DateTime

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps
import scala.util.{Failure, Random, Success, Try}
import scalaz.{-\/, \/, \/-}

trait ErrorLogging extends LazyLogging {
  def errorLogging[A](info: String, context: String = "")(block: => A): A = Try(block) match {
    case Success(result) =>
      result
    case Failure(e) =>
      logger.error(s"$info \n \n $context", e)
      throw e
  }
}

sealed trait Result extends Product with Serializable
case class ExperimentFailure(name: String, e: Throwable) extends Result
case class Match[A](control: A, candidate: A, name: String) extends Result
case class MisMatch[A](control: A, candidate: A, name: String, diff: String) extends Result
case object SkipResult extends Result

object Result {
  def fromDiffResult[A](control: A, candidate: A, name: String, diff: Comparison): Result = {
    if (diff.isIdentical) {
      Match(control, candidate, name)
    } else {
      MisMatch(control, candidate, name, diff.string)
    }
  }
}

object Experiment extends LazyLogging with ErrorLogging {

  private implicit val blockingContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  val experimentDelay: FiniteDuration = 0.seconds

  private def logResult[A](result: Result): Unit = result match {
    case ExperimentFailure(name, e) =>
      logger.error(s"Experiment $name error encountered", e)
    case MisMatch(control, candiate, name, diff) =>
      logger.warn(s"Experiment $name failed \n $control \n is not \n $candiate \n \n $diff")
    case Match(_, _, name) =>
      logger.info(s"Experiment $name succeeded")
    case SkipResult => ()
  }

  private def diffExperiment[A](name: String,
                                control: => Future[A],
                                candidate: => Future[A],
                                retries: Int,
                                retryControl: => Option[Future[A]])
                               (implicit
                                actorSystem: ActorSystem,
                                diffShow: DiffShow[A],
                                skipPredicate: SkipPredicate[A]): Future[Unit] = {
    def applyDiff(): Future[Result] = {
      for {
        controlValue <- control
        candidateValue <- candidate
      } yield errorLogging("diffExperiment applyDiff", s"$controlValue \n \n $candidateValue") {
        if (skipPredicate.skip(controlValue) || skipPredicate.skip(candidateValue))
          SkipResult
        else
          Result.fromDiffResult(
            controlValue,
            candidateValue,
            name,
            diffShow.diff(controlValue, candidateValue)
          )
      }
    }

    applyDiff()
      .recover { case e => ExperimentFailure(name, e) }
      .map {
        case m @ Match(_, _, _) => Experiment.logResult(m)
        case _ if retries > 0 =>
          lazy val newControl = retryControl.getOrElse(control)
          delayedBlocking(name, newControl, candidate, retries - 1, retryControl, 10 seconds)
          ()
        case r => Experiment.logResult(r)
      }
  }

  def delayedBlocking[A](name: String,
                         control: => Future[A],
                         candidate: => Future[A],
                         retries: Int = 0,
                         retryControl: => Option[Future[A]] = None,
                         candidateDelay: FiniteDuration = experimentDelay)
                        (implicit
                         actorSystem: ActorSystem,
                         diffShow: DiffShow[A],
                         skipPredicate: SkipPredicate[A]): Future[Unit] = {

    after(candidateDelay, actorSystem.scheduler) {
      diffExperiment(name, control, candidate, retries, retryControl)
    }
  }
}



trait SkipPredicate[A] {
  def skip(v: A): Boolean
}

object SkipPredicate {
  val skipBound: Int = 100059326
  def build[A](f: A => Boolean): SkipPredicate[A] = new SkipPredicate[A] {
    override def skip(v: A): Boolean = f(v)
  }
  def skip[A](v: A)(implicit skipPredicate: SkipPredicate[A]): Boolean = skipPredicate.skip(v)
}

trait LowPriorityImplicits {
  implicit def defaultSkip[A]: SkipPredicate[A] = SkipPredicate.build(_ => false)
}

trait HighPriorityImplicits extends LowPriorityImplicits {

  implicit def optionSkip[A](implicit sp: SkipPredicate[A]): SkipPredicate[Option[A]] = SkipPredicate.build(v => v.flatMap(Option.apply).exists(sp.skip))

  implicit def eitherSkip[A,B](implicit es1: SkipPredicate[A], es2: SkipPredicate[B]): SkipPredicate[\/[A, B]] = SkipPredicate.build {
    case \/-(x) => es2.skip(x)
    case -\/(x) => es1.skip(x)
  }

  implicit val userSkip: SkipPredicate[User] = SkipPredicate.build(u => u.id > SkipPredicate.skipBound.toString)

  implicit def localTimeDiffShow: DiffShow[DateTime] = new DiffShow[DateTime] {
    def show(d: DateTime): String = d.getMillis.toString
    def diff(l: DateTime, r: DateTime): Comparison = {
      if (math.abs(l.getMillis - r.getMillis) > 10000)
        Different(l, r)
      else
        Identical(l)
    }
  }

   implicit def unorderedIterableDiff[A](implicit ed: DiffShow[A]): DiffShow[Iterable[A]] = new DiffShow[Iterable[A]] {
     override def show(t: Iterable[A]): String = t.toSeq.toString()

     override def diff(left: Iterable[A], right: Iterable[A]): Comparison = {
       val leftSeq = left.toSeq.sortBy(_.toString)
       val rightSeq = right.toSeq.sortBy(_.toString)

       if (left.size != right.size) {
         Different(leftSeq, rightSeq)
       } else {
         leftSeq
           .zip(rightSeq)
           .find(p => !ed.diff(p._1, p._2).isIdentical)
           .fold(Identical(leftSeq): Comparison)(_ => Different(leftSeq, rightSeq))
       }
     }
   }
}

object implicits extends HighPriorityImplicits
