package models.database.mongo

import javax.inject.{Inject, Singleton}

import com.gu.identity.util.Logging
import models.client.{ApiError, ApiResponse, SearchResponse}
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.ReadPreference
import reactivemongo.api.commands.WriteResult
import reactivemongo.bson.BSONDocument
import reactivemongo.play.json._
import reactivemongo.play.json.collection._

import scalaz.std.scalaFuture._
import scala.concurrent.{ExecutionContext, Future}
import scalaz.{-\/, EitherT, \/-}

@Singleton class DeletedUsersRepository @Inject()(
    reactiveMongoApi: ReactiveMongoApi)(implicit ec: ExecutionContext) extends Logging {

  private lazy val reservedEmailsF = reactiveMongoApi.database.map(_.collection("reservedEmails"))

  def findBy(query: String): ApiResponse[Option[DeletedUser]] =
    reservedEmailsF.flatMap {
      _.find(buildSearchQuery(query))
       .cursor[DeletedUser](ReadPreference.primaryPreferred)
       .headOption.map(\/-(_))
    }.recover { case error =>
      val title = s"Failed to perform search in MongoDB"
      logger.error(title, error)
      -\/(ApiError(title, error.getMessage))
    }

  def search(query: String): ApiResponse[SearchResponse] =
    EitherT(findBy(query))
      .map({
          case Some(user) => SearchResponse.create(1, 0, List(IdentityUser(user.email, user.id)))
          case None => SearchResponse.create(0, 0, Nil)
        })
      .run

  def insert(id: String, email: String, username: String): Future[WriteResult] =
    reservedEmailsF.flatMap(_.insert[DeletedUser](DeletedUser(id, email, username)))

  def remove(id: String): Future[WriteResult] =
    reservedEmailsF.flatMap(_.remove(BSONDocument("_id" -> id), firstMatchOnly = true))

  private def buildSearchQuery(query: String) =
    Json.obj(
      "$or" -> Json.arr(
        Json.obj("_id" -> query.toLowerCase),
        Json.obj("email" -> query.toLowerCase),
        Json.obj("username" -> query)
      )
    )
}
