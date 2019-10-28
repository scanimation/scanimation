package scanimation.util

import java.lang.System.currentTimeMillis

import scanimation.common.Writeable

object counter {

  /** Counts the number of updates per second */
  class Counter(tps: Writeable[Int]) {
    private var count: Int = 0
    private var measure: Long = currentTimeMillis

    /** Increments the counter or flushes it to the writable */
    def update(): Unit = {
      count = count + 1
      val now = currentTimeMillis
      if (now > measure) {
        measure = now + 1000
        tps.write(count)
        count = 0
      }
    }
  }

}