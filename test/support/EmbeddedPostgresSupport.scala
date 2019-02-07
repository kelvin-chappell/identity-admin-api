package support

import java.nio.file.Paths

import de.flapdoodle.embed.process.config.IRuntimeConfig
import models.database.postgres.PostgresJsonFormats
import org.scalatest.{BeforeAndAfterAll, Suite}
import ru.yandex.qatools.embed.postgresql.EmbeddedPostgres
import ru.yandex.qatools.embed.postgresql.distribution.Version
import scalikejdbc._

import scala.collection.JavaConverters._

trait EmbeddedPostgresSupport extends BeforeAndAfterAll {
  self: Suite =>

  lazy val postgres = new EmbeddedPostgres(Version.V9_6_3)

  private lazy val embeddedPostgresConfig: IRuntimeConfig =
    EmbeddedPostgres.cachedRuntimeConfig(Paths.get(System.getProperty("user.home"), ".embedpostgresql"))

  def startPostgres(): String =
    postgres.start(embeddedPostgresConfig, "localhost", 5431, "identitydb", "username", "password", List.empty.asJava)

  def stopPostgres(): Unit = postgres.stop()

  def createTables(tableNames: String*): Unit = DB.localTx { implicit s =>
    tableNames.foreach { tableToCreate =>
      val create =
      s"""
           |DROP TABLE IF EXISTS $tableToCreate;
           |CREATE TABLE $tableToCreate(
           |  id VARCHAR(36) NOT NULL PRIMARY KEY,
           |  jdoc jsonb NOT NULL
           |);
           |
           |CREATE INDEX idx_$tableToCreate ON $tableToCreate USING GIN (jdoc);
         """.stripMargin
      SQL(create).update().apply()
    }

    val newsletterSubscriptionTable = """
      |DROP TABLE IF EXISTS newsletter_subscriptions;
      |CREATE TABLE IF NOT EXISTS newsletter_subscriptions(
      |  user_id VARCHAR NOT NULL REFERENCES users(id),
      |  newsletter_name VARCHAR NOT NULL,
      |  subscribed BOOLEAN NOT NULL,
      |  last_updated TIMESTAMP NOT NULL,
      |  PRIMARY KEY(user_id, newsletter_name)
      |);
    """.stripMargin
    SQL(newsletterSubscriptionTable).update().apply()

    val autoSignInTokensTable = """
      |DROP TABLE IF EXISTS autosignintokens;
      |CREATE TABLE IF NOT EXISTS autosignintokens(
      |  token VARCHAR PRIMARY KEY,
      |  identity_id VARCHAR NOT NULL REFERENCES users(id),
      |  email_address VARCHAR NOT NULL,
      |  created TIMESTAMP NOT NULL,
      |  has_been_used BOOLEAN NOT NULL,
      |  has_been_invalidated BOOLEAN NOT NULL
      |);
    """.stripMargin
    SQL(autoSignInTokensTable).update().apply()
  }

  override def beforeAll: Unit = {
    super.beforeAll()
    val url = startPostgres()
    ConnectionPool.singleton(url, "username", "password")
    createTables("users", "reservedusernames", "reservedemails", "accesstokens", "guestregistrationrequests", "syncedPrefs", "reservedemails", "passwordhashes", "passwordhashes", "autosignintokens")
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        stopPostgres()
      }
    })
  }

  override def afterAll: Unit = {
    stopPostgres()
    super.afterAll()
  }
}

trait PgTestUtils extends PostgresJsonFormats {

  def execSql(sql: SQL[Nothing, NoExtractor]): Int =
    DB.localTx { implicit session =>
      sql.update().apply()
    }

  def select[T](sql: SQL[T, HasExtractor]): Option[T] =
    DB.localTx { implicit session =>
      sql.single().apply()
    }

  def selectAll[T](sql: SQL[T, HasExtractor]): List[T] =
    DB.localTx { implicit session =>
      sql.list().apply()
    }

}