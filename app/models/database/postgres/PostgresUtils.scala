package models.database.postgres

import actors.metrics.MetricsSupport.Namespace
import com.gu.identity.util.Logging
import models.client.{ApiError, ApiResponse}
import scalikejdbc.{DB, DBSession}

import scala.concurrent.{ExecutionContext, Future}
import scalaz.\/

trait PostgresUtils {
  self: Logging =>

  implicit val namespace: Namespace = Namespace("Postgres-repo")

  def logFailure(msg: String): Throwable => ApiError = { t =>
    logger.error(msg, t)
    ApiError(msg, t.getMessage)
  }

  def readOnly[T](f: DBSession => T)
                 (recover: Throwable => ApiError)
                 (implicit ec: ExecutionContext): ApiResponse[T] = Future {
    \/.fromTryCatchNonFatal(DB.readOnly(f)).leftMap(recover)
  }

  def localTx[T](f: DBSession => T)
                (recover: Throwable => ApiError)
                (implicit ec: ExecutionContext): ApiResponse[T] = Future {
    \/.fromTryCatchNonFatal(DB.localTx(f)).leftMap(recover)
  }

}
