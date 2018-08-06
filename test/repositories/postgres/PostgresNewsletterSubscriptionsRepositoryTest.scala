package repositories.postgres

import actors.MetricsActorProviderStub
import models.database.postgres.PostgresNewsletterSubscriptionsRepository
import org.scalatest.{DoNotDiscover, FunSuite, Matchers}
import support.PgTestUtils
import scalikejdbc._

@DoNotDiscover
class PostgresNewsletterSubscriptionsRepositoryTest extends FunSuite with PgTestUtils with Matchers {

  val repo = new PostgresNewsletterSubscriptionsRepository(MetricsActorProviderStub)

  test("unsubscribeAll sets all subscriptions to unsubscribed") {
    execSql(sql"DELETE FROM newsletter_subscriptions")
    execSql(sql"DELETE FROM users")

    execSql(sql"INSERT INTO users values('user_id', '{}'::jsonb)")
    execSql(sql"INSERT INTO newsletter_subscriptions values('user_id', 'newsletter_name', TRUE, '2018-01-01')")
    execSql(sql"INSERT INTO newsletter_subscriptions values('user_id', 'newsletter_name2', TRUE, '2018-01-01')")

    repo.unsubscribeAll("user_id")

    selectAll(sql"SELECT subscribed FROM newsletter_subscriptions".map(_.boolean("subscribed"))) shouldBe List(false, false)
  }

}
