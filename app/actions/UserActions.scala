package actions

import models.client.ClientJsonFormats._
import javax.inject.{Inject, Singleton}

import com.gu.tip.Tip
import configuration.Config
import models.client.{GuardianUser, User}
import play.api.mvc.{ActionRefiner, Request, Result, WrappedRequest}
import play.api.mvc.Results._
import services.{SalesforceService, UserService}

import scala.concurrent.{ExecutionContext, Future}
import scalaz.std.scalaFuture._
import scalaz.{-\/, EitherT, \/, \/-}

class UserRequest[A](val user: GuardianUser, request: Request[A]) extends WrappedRequest[A](request)

@Singleton class IdentityUserAction @Inject() (userService: UserService)(implicit ec: ExecutionContext) {

  def apply(userId: String) = new ActionRefiner[Request, UserRequest] {
    def executionContext = ec

    override def refine[A](request: Request[A]): Future[Either[Result, UserRequest[A]]] = {

      def findUserById(userId: String): Future[Result \/ GuardianUser] =
        EitherT(userService.findById(userId)).fold(
          error => -\/(InternalServerError(error)),
          {
            case Some(user) => \/-(user)
            case None => -\/(NotFound)
          }
        )

      def enrichUserWithProducts(user: GuardianUser): Future[Result \/ GuardianUser]  =
        EitherT(userService.enrichUserWithProducts(user)).leftMap(InternalServerError(_)).run

      def addBanStatus(user: GuardianUser) =
        EitherT(userService.enrichUserWithBannedStatus(user)).leftMap(InternalServerError(_)).run

      (for {
        user <- EitherT(findUserById(userId))
        userWithProducts <- EitherT(enrichUserWithProducts(user))
        userWithBanStatus <- EitherT(addBanStatus(userWithProducts))
      } yield {
        if (Config.stage == "PROD") Tip.verify("User Retrieval")
        new UserRequest(userWithBanStatus, request)
      }).run.map(_.toEither)

    }
  }

}

@Singleton class OrphanUserAction @Inject() (salesforce: SalesforceService)(implicit ec: ExecutionContext)  {

  def apply(email: String) = new ActionRefiner[Request, UserRequest] {
    def executionContext = ec

    override def refine[A](input: Request[A]): Future[Either[Result, UserRequest[A]]] = {
      val orphanEitherT =
        for {
          subOrphanOpt <- EitherT(salesforce.getSubscriptionByEmail(email))
        } yield {

          val userRequest = Some(new UserRequest(
            GuardianUser(
              idapiUser = User(id = "orphan", email = email),
              orphan = true,
              subscriptionDetails = subOrphanOpt
            ),
            input)
          )

          if (subOrphanOpt.isDefined)
            userRequest
          else
            None
        }

      orphanEitherT.fold(
        error => Left(InternalServerError(error)),
        orphanOpt => orphanOpt.fold[Either[Result, UserRequest[A]]]
          (Left(NotFound))
          (orphan => Right(orphan))
      )
    }
  }

}



