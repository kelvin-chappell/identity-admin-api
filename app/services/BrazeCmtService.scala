package services
import com.typesafe.scalalogging.LazyLogging
import javax.inject.Inject
import models.client.{ApiResponse, EmailSubscriptionStatus, NewslettersSubscription}
import scalaz.EitherT
import scalaz._, Scalaz._
import scala.concurrent.ExecutionContext

class BrazeCmtService @Inject() (identityApiClient: IdentityApiClient)(implicit ec: ExecutionContext)
  extends CmtService
  with LazyLogging {

  override def findUserSubscriptions(userId: String): ApiResponse[EmailSubscriptionStatus] = {
    for {
      lists <- EitherT(identityApiClient.getUserLists(userId))
      emailSub = lists.headOption.map(_ => NewslettersSubscription(lists.map(_.listId.toString)))
    } yield EmailSubscriptionStatus("ok", emailSub, None)
  }.run

  override def unsubscribeAll(userId: String, email: String): ApiResponse[Unit] =
    identityApiClient.deleteAllLists(userId)

  override def updateEmailAddress(identityId: String, oldEmail: String, newEmail: String): ApiResponse[Unit] =
    identityApiClient.changeEmail(identityId, newEmail)
}
