package util

import java.time.Instant

class StopWatch {
  private val startTime = Instant.now().toEpochMilli
  def elapsed: Long = Instant.now().toEpochMilli - startTime
}
