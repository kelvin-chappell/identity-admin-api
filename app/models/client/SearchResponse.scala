package models.client

import models.database.mongo.PersistedUser
import play.api.libs.json.Json
import play.api.mvc.{Result, Results}

import scala.language.implicitConversions

case class SearchResponse(total: Int,
                          hasMore: Boolean,
                          results: Seq[UserSummary] = Nil)

object SearchResponse {
  implicit val format = Json.format[SearchResponse]
  
  implicit def searchResponseToResult(searchResponse: SearchResponse): Result =
    Results.Ok(Json.toJson(searchResponse))

  def create(total: Int, offset: Int, results: Seq[PersistedUser]): SearchResponse = {
    val hasMore = (offset + results.size) < total
    SearchResponse(total, hasMore, results.map(UserSummary.fromPersistedUser))
  }
}
