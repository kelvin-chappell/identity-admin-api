package repositories.postgres

import org.scalatest.Suites
import support.EmbeddedPostgresSupport

// Combined suite for tests that require embedded postgres
// Ensures the nested suites are not run in parallel and we only start postgres once
class EmbeddedPostgresSuites extends Suites(
  new PostgresDeletedUserRepositoryTest,
  new PostgresReservedUsernameRepositoryTest,
  new PostgresUserRepositoryTest
) with EmbeddedPostgresSupport
