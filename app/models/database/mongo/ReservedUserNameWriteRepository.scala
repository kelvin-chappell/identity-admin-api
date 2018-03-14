package models.database.mongo

import javax.inject.{Inject, Singleton}

import com.gu.identity.util.Logging
import models.client
import models.client.{ApiError, ApiResponse, ReservedUsername, ReservedUsernameList}
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.{Cursor, ReadPreference}
import reactivemongo.play.json._
import reactivemongo.play.json.collection._

import scala.concurrent.ExecutionContext
import scalaz.std.scalaFuture._
import scalaz.{-\/, OptionT, \/-}

// TODO implement for postgres
@Singleton class ReservedUserNameWriteRepository @Inject() (
    environment: play.api.Environment,
    reactiveMongoApi: ReactiveMongoApi)(implicit ec: ExecutionContext) extends Logging {

  private lazy val reservedUsernamesF = reactiveMongoApi.database.map(_.collection("reservedUsernames"))

  private def buildSearchQuery(query: String) =
    Json.obj(
      "$or" -> Json.arr(
        Json.obj("_id" -> query.toLowerCase),
        Json.obj("primaryEmailAddress" -> query.toLowerCase),
        Json.obj("username" -> query)
      )
    )

  def removeReservedUsername(username: String): ApiResponse[ReservedUsernameList] =
    reservedUsernamesF
      .flatMap(_.remove(buildSearchQuery(username)))
      .flatMap(_ => loadReservedUsernames)
      .recover { case error =>
        val title = s"Failed to remove reserved username $username"
        logger.error(title, error)
        -\/(ApiError(title, error.getMessage))
      }

  def loadReservedUsernames: ApiResponse[ReservedUsernameList] = {
    val listOfReservedUsernamesF = reservedUsernamesF.flatMap {
       _.find(Json.obj())
        .sort(Json.obj("username" -> 1))
        .cursor[ReservedUsername](ReadPreference.primaryPreferred)
        .collect[List](-1, Cursor.FailOnError[List[ReservedUsername]]())
    }

    (for {
      listOfReservedUsernames <- listOfReservedUsernamesF
      listOfUsernames = listOfReservedUsernames.map(_.username)
    } yield {
      \/-(client.ReservedUsernameList(listOfUsernames))
    }).recover { case error =>
      val title = "Failed to load reserved usernames list"
      logger.error(title, error)
      -\/(ApiError(title, error.getMessage))
    }
  }

  def addReservedUsername(username: String): ApiResponse[ReservedUsernameList] =
    reservedUsernamesF
      .flatMap(_.insert[ReservedUsername](ReservedUsername(username)))
      .flatMap(_ => loadReservedUsernames)
      .recover { case error =>
        val title = s"Failed to add $username to reserved username list"
        logger.error(title, error)
        -\/(ApiError(title, error.getMessage))
      }
}
