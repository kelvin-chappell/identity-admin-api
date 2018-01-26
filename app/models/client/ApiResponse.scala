package models

import scala.concurrent.Future
import scalaz.\/

package object client {
  type ApiResponse[T] = Future[ApiError \/ T]
}
