package services

import com.amazonaws.services.simpleemail._
import com.amazonaws.services.simpleemail.model._
import com.typesafe.scalalogging.LazyLogging
import configuration.Config
import scala.util.{Failure, Success, Try}

object EmailService extends LazyLogging {
  private val client = AmazonSimpleEmailServiceAsyncClientBuilder.standard()
      .withCredentials(Config.AWS.credentialsProvider)
      .withRegion(Config.AWS.region)
      .build()

  private object DeletionEmail {
    val FROM = "userhelp@theguardian.com"
    val SUBJECT = "Guardian account deletion confirmation"

    val BODY =
      s"""
       | Your account has been deleted from theguardian.com.
       |
       | If you are signed into theguardian.com, please sign out for this change to take effect.
       |
       | Also, please note that it can take up to two working days to be removed from our mailing lists.
       |
       | If you have any other questions, please reply to this email and we will be happy to help.
       |
       | Kind regards,
       |
       | Guardian Userhelp
       |
       | Guardian News and Media Ltd
       | Kings Place, 90 York Way
       | London N1 9GU
     """.stripMargin

    val subject = new Content().withData(SUBJECT)
    val textBody = new Content().withData(BODY)
    val body = new Body().withText(textBody)
    val message = new Message().withSubject(subject).withBody(body)
  }

  def sendDeletionConfirmation(toEmailAddress: String) = {
    val destination = new Destination().withToAddresses(toEmailAddress)
    val request = new SendEmailRequest()
        .withSource(DeletionEmail.FROM)
        .withDestination(destination)
        .withMessage(DeletionEmail.message)


    Try(client.sendEmail(request)) match {
      case Success(_) => logger.info("Sending account deletion confirmation email")
      case Failure(e) => logger.error("Could not send account deletion confirmation email", e)
    }
  }
}
