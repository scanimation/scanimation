package scanimation.util

import org.querki.jquery._
import org.scalajs.dom._

import scala.concurrent.{Future, Promise}

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

  object Timer {
    /** Schedules a new timer that executes given code with given tps */
    def schedule(tps: Double, code: () => Unit): Unit = {
      new Timer().start(tps, code)
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
    /** Internal promise to be used for futures */
    private val promise = Promise[Unit]

    /** Starts the awaiter with given condition and code, which will execute code once the condition is true */
    def start(condition: () => Boolean, code: () => Unit): Unit = {
      window.requestAnimationFrame(_ => update(condition, code))
    }

    /** Checks the condition and schedules the next execution */
    def update(condition: () => Boolean, code: () => Unit): Unit = {
      if (condition.apply()) {
        code.apply()
        promise.success()
      } else {
        window.requestAnimationFrame(_ => update(condition, code))
      }
    }

    /** Returns a future that will be resolved once awaitor completes */
    def future: Future[Unit] = promise.future
  }

  object Awaitor {
    /** Executes the given code once the given condition is met */
    def await(condition: => Boolean)(code: => Unit): Awaitor = {
      val awaitor = new Awaitor()
      awaitor.start(() => condition, () => code)
      awaitor
    }

    /** Executes the given code once the given selector returns one or more elements */
    def onceExists(selector: String)(code: (String, JQuery) => Unit): Awaitor = {
      Awaitor.await($(selector).length > 0)(code.apply(selector, $(selector)))
    }
  }

}