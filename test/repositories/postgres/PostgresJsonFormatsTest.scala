package repositories.postgres

import models.database.postgres.PostgresJsonFormats
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.{Matchers, WordSpecLike}
import play.api.libs.json.{JsString, JsSuccess}

class PostgresJsonFormatsTest extends WordSpecLike
  with Matchers {

  trait TestFixture extends PostgresJsonFormats

  "Postgres Json Formats" should {

    "Support reading ISO-8601 date time strings" in new TestFixture {
      val JsSuccess(actual, _) = dateTimeRead.reads(JsString("2017-11-16T09:54:21.123Z"))
      val expected = DateTime.parse("2017-11-16T09:54:21.123Z")
      actual.compareTo(expected) shouldBe 0
    }

    "Write dates adhering to the ISO Spec" in new TestFixture {
      dateTimeWrite.writes(new DateTime(42000l, DateTimeZone.UTC)) shouldBe JsString("1970-01-01T00:00:42.000Z")
    }

    "Read written dates" in new TestFixture {
      val expectedDate = new DateTime(42000l, DateTimeZone.UTC)
      dateTimeRead.reads(dateTimeWrite.writes(expectedDate)) shouldBe JsSuccess(expectedDate)
    }
  }
}
