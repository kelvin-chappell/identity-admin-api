package services

import javax.inject.{Inject, Singleton}

import com.typesafe.scalalogging.LazyLogging
import configuration.Config
import models.client.{ApiError, ApiResponse}
import play.api.http.Status
import play.api.libs.json.Json
import play.api.libs.ws.WSClient

import scala.concurrent.ExecutionContext
import scalaz.{-\/, \/-}

case class DiscussionStats(status: String, comments: Int, pickedComments: Int)

object DiscussionStats {
  implicit val format = Json.format[DiscussionStats]
}

@Singleton class DiscussionService @Inject() (ws: WSClient)(implicit ec: ExecutionContext) extends LazyLogging {
  def hasCommented(id: String): ApiResponse[Boolean] = {
    ws.url(s"${Config.Discussion.apiUrl}/profile/$id/stats").get().map { response =>
      if (response.status == Status.NOT_FOUND) {
        \/-(false)
      } else {
        Json.parse(response.body).validate[DiscussionStats].fold(
          error =>  {
            logger.error(response.body)
            -\/(ApiError("Failed to determine if user has commented due to Json parsing error", error.toString))

          },
          discussionStats => \/-(discussionStats.comments > 0)
        )
      }
    }.recover { case error =>
      logger.error("Failed to communicate with DAPI", error.getMessage)
      -\/(ApiError("Failed to communicate with DAPI", error.getMessage))
    }
  }
}

