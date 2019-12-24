package scanimation.util

import org.querki.jquery._
import org.scalajs.dom._

object timer {

  class Timer {
    /** Starts the timer to tick with given tps - ticks per seconds */
    def start(tps: Double, code: () => Unit): Unit = {
      val startTime = System.currentTimeMillis()
      code.apply()
      val elapsedTime = System.currentTimeMillis() - startTime
      val delay = (1000 / tps - elapsedTime) max 0
      window.setTimeout(() => start(tps, code), delay)
    }
  }

  class Animator {
    /** Starts the timer to call the code on each animation frame */
    def start(code: () => Unit): Unit = {
      window.requestAnimationFrame(_ => update(code))
    }

    /** Executes the code and schedules next execution */
    def update(code: () => Unit): Unit = {
      code.apply()
      window.requestAnimationFrame(_ => update(code))
    }
  }

  class Awaitor {
    /** Starts the awaiter with given condition and code, which will execute code once the condition is true */
    def start(condition: () => Boolean, code: () => Unit): Unit = {
      window.requestAnimationFrame(_ => update(condition, code))
    }

    /** Checks the condition and schedules the next execution */
    def update(condition: () => Boolean, code: () => Unit): Unit = {
      if (condition.apply()) {
        code.apply()
      } else {
        window.requestAnimationFrame(_ => update(condition, code))
      }
    }
  }

  object Awaitor {
    /** Executes the given code once the given condition is met */
    def await(condition: => Boolean)(code: => Unit): Unit = {
      new Awaitor().start(() => condition, () => code)
    }

    /** Executes the given code once the given selector returns one or more elements */
    def onceExists(selector: String)(code: (String, JQuery) => Unit): Unit = {
      Awaitor.await($(selector).length > 0)(code.apply(selector, $(selector)))
    }
  }

}