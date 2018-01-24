package services

import javax.inject.{Inject, Singleton}

import com.exacttarget.fuelsdk._
import com.gu.identity.util.Logging
import configuration.Config
import models._
import models.client._
import models.database.mongo.UsersReadRepository

import scala.concurrent.{ExecutionContext, Future}
import scalaz.std.scalaFuture._
import scalaz.{-\/, EitherT, \/, \/-}
import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}

@Singleton class ExactTargetService @Inject() (
    usersReadRepository: UsersReadRepository)(implicit ec: ExecutionContext) extends Logging {
  /**
    * Unsubscribe this subscriber from all current and future subscriber lists.
    */
  def unsubscribeFromAllLists(email: String): ApiResponse[Unit] = {
    val adminStatusUpdateF = EitherT(updateSubscriptionStatus(email, ETSubscriber.Status.UNSUBSCRIBED, etClientAdmin))
    val editorialStatusUpdateF = EitherT(updateSubscriptionStatus(email, ETSubscriber.Status.UNSUBSCRIBED, etClientEditorial))

    (for {
      _ <- adminStatusUpdateF
      _ <- editorialStatusUpdateF
    } yield {}).run
  }

  /**
    * Activates subscriber in both Admin and Editorial business units.
    */
  def activateEmailSubscription(email: String): ApiResponse[Unit] = {
    val adminStatusUpdateF = EitherT(updateSubscriptionStatus(email, ETSubscriber.Status.ACTIVE, etClientAdmin))
    val editorialStatusUpdateF = EitherT(updateSubscriptionStatus(email, ETSubscriber.Status.ACTIVE, etClientEditorial))

    (for {
      _ <- adminStatusUpdateF
      _ <- editorialStatusUpdateF
    } yield {}).run
  }

  def updateEmailAddress(oldEmail: String, newEmail: String): ApiResponse[Unit] =
    EitherT(retrieveSubscriber(oldEmail, etClientAdmin)).flatMap {
      case Some(subscriber) =>
        subscriber.setEmailAddress(newEmail)
        EitherT(updateSubscriber(subscriber, etClientAdmin))

      case None => EitherT.right(Future.successful({}))
    }.run

  def newslettersSubscriptionByIdentityId(identityId: String): ApiResponse[Option[NewslettersSubscription]] =
    EitherT(usersReadRepository.find(identityId)).flatMap {
      case Some(user) => EitherT(newslettersSubscriptionByEmail(user.email))
      case None => EitherT.right(Future.successful(Option.empty[NewslettersSubscription]))
    }.run

  def newslettersSubscriptionByEmail(email: String): ApiResponse[Option[NewslettersSubscription]] = {

    def activeNewsletterSubscriptions(subscriptions: List[ETSubscriber#Subscription]) =
      subscriptions.filter(_.getStatus == ETSubscriber.Status.ACTIVE).map(_.getListId)

    def subscriberIsActive(subscriber: ETSubscriber) = subscriber.getStatus == ETSubscriber.Status.ACTIVE

    EitherT(retrieveSubscriber(email, etClientEditorial)).map {
      case Some(subscriber) => {
        val activeList = activeNewsletterSubscriptions(subscriber.getSubscriptions.toList)

        if (subscriberIsActive(subscriber) && !activeList.isEmpty)
          Some(NewslettersSubscription(activeList))
        else
          None
      }

      case None => None
    }.run
  }

  /**
    * In Consumer BU:
    *   Data Extension:   'All Emails Sent' B2FD61EE-00C2-44A2-B10F-3D7DD308D9CE
    *   SQL Query:        'All Emails Sent' 5ca82872-97ee-42d9-8764-38a2da937a7b
    *   Automation:       'All Emails Sent' 4c4ea8e4-f541-5aa7-591e-0bbcd3327ea8
    *   API Integration:  'All Emails Sent' 82e38185-322d-438b-a9f6-63f2cd37e252
    */
  def consumerEmailsByEmail(email: String): ApiResponse[Option[ConsumerEmails]] = {

    val recordsFromDeT: EitherT[Future, ApiError, Option[List[ETDataExtensionRow]]] =

      EitherT(retrieveDataExtension("B2FD61EE-00C2-44A2-B10F-3D7DD308D9CE", etClientConsumer)).flatMap {
        case Some(de) =>
          EitherT(selectFromDataExtension(s"SubscriberKey=$email", de))

        case None => EitherT.right(Future.successful(None))
      }

    recordsFromDeT.map {
      case Some(rows) =>
        val consumerEmails = rows.map { row =>
          ConsumerEmail(
            row.getColumn("EmailName"),
            row.getColumn("DeliveredTime"),
            row.getColumn("OpenTime")
          )
        }

        Some(ConsumerEmails(consumerEmails))

      case None => None
    }.run
  }

  def deleteSubscriber(email: String): ApiResponse[Unit] =
    EitherT(retrieveSubscriber(email, etClientAdmin)).flatMap {
      case Some(subscriber) => EitherT(deleteSubscriber(subscriber))
      case None => EitherT.right(Future.successful({}))
    }.run

  /*
   Currently (12.07.2017) there is a bug where if the user is unsubscribed at Editorial Business Unit level,
   they cannot re-subscribe from the article page such as:
   https://www.theguardian.com/info/2015/dec/08/daily-email-uk

   They can re-subscribe from the email-prefs page if they are signed in.

   This ugly method considers the user active only if they are active in both business units

   If the user exists only in the Admin Business Unit, then it returns whatever status is there.
   */
  def status(email: String): ApiResponse[Option[String]] = {
    val adminSubscriberOptF = EitherT(retrieveSubscriber(email, etClientAdmin))
    val editorialSubscriberOptF = EitherT(retrieveSubscriber(email, etClientEditorial))

    (for {
      adminSubscriberOpt <- adminSubscriberOptF
      editorialSubscriberOpt <- editorialSubscriberOptF
    } yield {

      def subscriberExistsInBothBusinessUnits: Boolean =
        List(adminSubscriberOpt, editorialSubscriberOpt).forall(_.isDefined)

      def subscriberIsActiveInBothBusinessUnits: Boolean =
        (for {
          adminSubscriber <- adminSubscriberOpt
          editorialSubscriber <- editorialSubscriberOpt
        } yield {
          List(adminSubscriber, editorialSubscriber).map(_.getStatus.value).forall(_ == "Active")
        }).getOrElse(false)

      def adminBusinessUnitStatus: Option[String] =
        adminSubscriberOpt match {
          case Some(subscriber) => subscriber.getStatus.value match {
            case "Active" => Some("Active")
            case _ => Some("Unsubscribed")
          }
          case None => None
        }

      if (subscriberExistsInBothBusinessUnits) {
        if (subscriberIsActiveInBothBusinessUnits)
          Some("Active")
        else
          Some("Unsubscribed")
      }
      else
        adminBusinessUnitStatus

    }).run
  }

  def subscriberByEmail(email: String): ApiResponse[Option[ExactTargetSubscriber]] = {
    val statusF = EitherT(status(email))
    val newslettersF = EitherT(newslettersSubscriptionByEmail(email))
    val consumerEmailsF = EitherT(consumerEmailsByEmail(email))

    val subByEmailT =
      for {
        statusOpt <- statusF
        newslettersOpt <- newslettersF
        consumerEmailsOpt <- consumerEmailsF
      } yield {
        statusOpt match {
          case Some(status) => Some(ExactTargetSubscriber(status, newslettersOpt, consumerEmailsOpt))
          case None => None
        }
      }

    subByEmailT.run
  }

  def subscriberByIdentityId(identityId: String): ApiResponse[Option[ExactTargetSubscriber]] =
    EitherT(usersReadRepository.find(identityId)).flatMap {
      case Some(user) => EitherT(subscriberByEmail(user.email))
      case None => EitherT.right(Future.successful(Option.empty[ExactTargetSubscriber]))
    }.run

  def contributionsByIdentityId(identityId: String): ApiResponse[List[Contribution]] =
    EitherT(usersReadRepository.find(identityId)).flatMap {
      case Some(user) => EitherT(contributionsByEmail(user.email))
      case None => EitherT.right(Future.successful(List.empty[Contribution]))
    }.run

  def contributionsByEmail(email: String): ApiResponse[List[Contribution]] = {
    val recordsFromDeT: EitherT[Future, ApiError, Option[List[ETDataExtensionRow]]] =

      EitherT(retrieveDataExtension("2AF90B07-0DE8-42A4-B549-3B808C77F56C", etClientAdmin)).flatMap {
        case Some(de) => EitherT(selectFromDataExtension(s"SubscriberKey=$email", de))

        case None => EitherT.right(Future.successful(None))
      }

    recordsFromDeT.map {
      case Some(rows) =>
        rows.map { row =>
          Contribution(
            row.getColumn("created"),
            row.getColumn("currency"),
            row.getColumn("amount")
          )
        }

      case None => Nil
    }.run
  }

  private def updateSubscriptionStatus(
    email: String, status: ETSubscriber.Status, client: ETClient): ApiResponse[Unit] = {

    def updateStatus(subscriber: ETSubscriber) = {
      subscriber.setStatus(status)
      updateSubscriber(subscriber, client)
    }

    def createAndUpdateStatus() = {
      val subscriber = new ETSubscriber()
      subscriber.setEmailAddress(email)
      subscriber.setKey(email)
      subscriber.setStatus(status)
      createSubscriber(subscriber)
    }

    EitherT(retrieveSubscriber(email, client)).flatMap {
      case Some(subscriber) => EitherT(updateStatus(subscriber))
      case None => EitherT(createAndUpdateStatus())
    }.run
  }

  private def retrieveSubscriber(email: String, client: ETClient): ApiResponse[Option[ETSubscriber]] = Future {

    val retrieveTry = Try {
      Option(client.retrieve(classOf[ETSubscriber], s"emailAddress=$email").getResult) match {
        case Some(result) => \/-(Some(result.getObject))
        case None => \/-(Option.empty[ETSubscriber])
      }
    }

    retrieveTry match {
      case Success(result) => result

      case Failure(error) =>
        val title = "Failed to retrieve subscriber from ExactTarget"
        logger.error(title, error)
        -\/(ApiError(title, error.getMessage))
    }
  }

  private def retrieveDataExtension(key: String, client: ETClient): ApiResponse[Option[ETDataExtension]] = Future {

    val retrieveTry = Try {
      Option(client.retrieve(classOf[ETDataExtension], s"key=$key").getResult) match {
        case Some(result) => \/-(Some(result.getObject))
        case None => \/-(Option.empty[ETDataExtension])
      }
    }

    retrieveTry match {
      case Success(result) => result

      case Failure(error) =>
        val title = s"Failed to retrieve DataExtension $key from ExactTarget"
        logger.error(title, error)
        -\/(ApiError(title, error.getMessage))
    }
  }

  private def selectFromDataExtension(
      query: String,
      de: ETDataExtension): ApiResponse[Option[List[ETDataExtensionRow]]] = Future {

    val retrieveTry = Try {
      Option(de.select(query).getObjects) match {
        case Some(results) => \/-(Some(results.toList))
        case None => \/-(Option.empty[List[ETDataExtensionRow]])
      }
    }

    retrieveTry match {
      case Success(result) => result

      case Failure(error) =>
        val title = s"Failed to retrieve DataExtension records from ExactTarget"
        logger.error(title, error)
        -\/(ApiError(title, error.getMessage))
    }
  }

  private def deleteSubscriber(subscriber: ETSubscriber): ApiResponse[Unit] = Future {
    val etResponse = etClientAdmin.delete(subscriber)
    handleETResponse(etResponse, "Failed to delete ExactTarget subscriber")
  }

  private def updateSubscriber(subscriber: ETSubscriber, client: ETClient): ApiResponse[Unit] = Future {
    val etResponse = client.update(subscriber)
    handleETResponse(etResponse, "Failed to update ExactTarget subscriber")
  }

  private def createSubscriber(subscriber: ETSubscriber): ApiResponse[Unit] = Future {
    val etResponse = etClientAdmin.create(subscriber)
    handleETResponse(etResponse, "Failed to create ExactTarget subscriber")
  }

  private def handleETResponse(etResponse: ETResponse[ETSubscriber], title: String): ApiError \/ Unit =
    if (etResponse.getResponseCode == "OK")
      \/-{}
    else {
      logger.error(s"${title}: ${etResponse.getResponseMessage}")
      -\/(ApiError(title, etResponse.getResponseMessage))
    }

  private lazy val etClientAdmin = {
    val etConf = new ETConfiguration()
    etConf.set("clientId", Config.ExactTarget.Admin.clientId)
    etConf.set("clientSecret", Config.ExactTarget.Admin.clientSecret)
    new ETClient(etConf)
  }

  private lazy val etClientEditorial = {
    val etConf = new ETConfiguration()
    etConf.set("clientId", Config.ExactTarget.Editorial.clientId)
    etConf.set("clientSecret", Config.ExactTarget.Editorial.clientSecret)
    new ETClient(etConf)
  }

  private lazy val etClientConsumer = {
    val etConf = new ETConfiguration()
    etConf.set("clientId", Config.ExactTarget.Consumer.clientId)
    etConf.set("clientSecret", Config.ExactTarget.Consumer.clientSecret)
    new ETClient(etConf)
  }
}