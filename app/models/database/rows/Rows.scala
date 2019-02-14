package models.database.rows

import java.sql.Timestamp

import play.api.libs.json.{JsString, Json, Writes}
import scalikejdbc.WrappedResultSet

case class NewsletterSubscriptionRow(userId: String, newsLetterName: String, subscribed: Boolean, lastUpdated: Timestamp)

object NewsletterSubscriptionRow {
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

case class AutoSignInTokenRow(identityId: String, emailAddress: String, created: Timestamp, isUsed: Boolean, isInvalidated: Boolean)

object AutoSignInTokenRow {
  implicit val timestampWrites: Writes[Timestamp] = timestamp => JsString(timestamp.toString)
  implicit val autoSignInTokenRowWrites = Json.writes[AutoSignInTokenRow]

  def apply(resultSet: WrappedResultSet): AutoSignInTokenRow = {
    AutoSignInTokenRow(
      resultSet.string("identity_id"),
      resultSet.string("email_address"),
      resultSet.timestamp("created"),
      resultSet.boolean("is_used"),
      resultSet.boolean("is_invalidated")
    )
  }
}