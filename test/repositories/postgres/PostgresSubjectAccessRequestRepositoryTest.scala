package repositories.postgres

import actors.MetricsActorProviderStub
import models.database.postgres.PostgresSubjectAccessRequestRepository
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover, Matchers, WordSpecLike}
import org.scalatest.concurrent.ScalaFutures
import support.PgTestUtils

@DoNotDiscover
class PostgresSubjectAccessRequestRepositoryTest extends WordSpecLike
  with Matchers
  with PgTestUtils
  with ScalaFutures
  with BeforeAndAfterAll {

  trait TestFixture {

    val repo = new PostgresSubjectAccessRequestRepository(MetricsActorProviderStub)

    implicit ec = repo.ec
  }

}
