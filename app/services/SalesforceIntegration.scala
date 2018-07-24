package services

import javax.inject._

import com.amazonaws.auth.AWSStaticCredentialsProvider
import com.amazonaws.services.sns.AmazonSNSClientBuilder
import com.amazonaws.services.sns.model.PublishResult
import com.typesafe.scalalogging.LazyLogging
import configuration.Config
import play.api.libs.json.Json

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

@Singleton class SalesforceIntegration @Inject() ()(implicit ec: ExecutionContext) extends LazyLogging {
  private val snsTopic = Config.IdentitySalesforceQueue.snsTopic
  private val snsEndpoint = Config.IdentitySalesforceQueue.snsEndPoint

  private val client = AmazonSNSClientBuilder.standard()
    .withCredentials(new AWSStaticCredentialsProvider(Config.IdentitySalesforceQueue.credentials))
    .withRegion(Config.AWS.region)
    .build()

  def constructMessage(userId: String, email: String): String = {
    Json.obj(
      "id" -> userId,
      "userDetails" -> Json.obj(
        "primaryEmailAddress" -> email
      )
    ).toString()
  }

  def enqueueUserUpdate(userId: String, email: String): Future[PublishResult] =
    Future {
      val message = constructMessage(userId, email)
      client.publish(snsTopic, message)
    }.andThen {
      case Success(_) => logger.info(s"Successfully enqueued $userId user update on $snsTopic @ $snsEndpoint")
      case Failure(e) => logger.error(s"Failed to enqueue $userId user update on $snsTopic @ $snsEndpoint", e)
    }
}
