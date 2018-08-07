package models.database.rows

import java.sql.Timestamp

import play.api.libs.json.{JsString, Json, Writes}
import scalikejdbc.WrappedResultSet

case class NewsletterSubscriptionRow(userId: String, newsLetterName: String, subscribed: Boolean, lastUpdated: Timestamp)

object NewsletterSubscriptionRow  {
  implicit val timestampWrites: Writes[Timestamp] = timestamp => JsString(timestamp.toString)
  implicit val newsletterSubscriptionRowWrites = Json.writes[NewsletterSubscriptionRow]

  def apply(resultSet: WrappedResultSet): NewsletterSubscriptionRow = {
    NewsletterSubscriptionRow(
      resultSet.string("user_id"),
      resultSet.string("newsletter_name"),
      resultSet.boolean("subscribed"),
      resultSet.timestamp("last_updated")
    )
  }
}