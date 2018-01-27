package actors

import actors.EventPublishingActor.{DisplayNameChanged, EmailValidationChanged}
import akka.actor.{Actor, Props}
import com.amazonaws.services.sns.AmazonSNSAsync
import com.amazonaws.services.sns.model.PublishRequest
import com.gu.identity.util.Logging
import configuration.Config
import net.liftweb.json.{DefaultFormats, NoTypeHints, Serialization}
import scala.util.{Failure, Success, Try}

object EventPublishingActor {

  implicit val formats = Serialization.formats(NoTypeHints)

  sealed trait UserChangedMessage

  case class EmailValidationChanged(userId: String) extends UserChangedMessage
  case class DisplayNameChanged(userId: String) extends UserChangedMessage

  def getProps(amazonSNSAsyncClient: AmazonSNSAsync) =
    Props(new EventPublishingActor(amazonSNSAsyncClient))

}

class EventPublishingActor(amazonSNSAsyncClient: AmazonSNSAsync) extends Actor with Logging {

  import net.liftweb.json.Serialization.write

  implicit val formats = DefaultFormats

  lazy val emailValidationChangedTopicArn = Config.PublishEvents.emailValidationChangedEventSnsArn
  lazy val displayNameChangedTopicArn = Config.PublishEvents.displayNameChangedEventSnsArn

  override def receive: Receive = {
    case emailValidationChanged: EmailValidationChanged =>
      logger.info(s"Sending email validation changed event to SNS for user ${emailValidationChanged.userId}")
      val subject = "E-mail Validation Changed"
      val message: String = write(emailValidationChanged)
      Try(amazonSNSAsyncClient.publishAsync(new PublishRequest(emailValidationChangedTopicArn, message, subject))) match {
        case Success(_) => logger.trace("Published to SNS message {}", message)
        case Failure(e) => logger.error(s"Could not publish event to E-mail validation changed SNS for user ID ${emailValidationChanged.userId}", e)
      }

    case displayNameChanged: DisplayNameChanged =>
      logger.info(s"Sending displayname changed event to SNS for user ${displayNameChanged.userId}")
      val subject = "Display Name Changed"
      val message: String = write(displayNameChanged)
      Try(amazonSNSAsyncClient.publishAsync(new PublishRequest(displayNameChangedTopicArn, message, subject))) match {
        case Success(_) => logger.trace("Published to SNS message {}", message)
        case Failure(e) => logger.error(s"Could not publish event to displayname changed SNS for user ID ${displayNameChanged.userId}", e)
      }

    case unknown =>
      logger.error(s"Received unsupported event in EventPublishingActor: $unknown")
  }
}
