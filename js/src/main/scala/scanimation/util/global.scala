package scanimation.util

import org.scalajs.dom.window

import scala.concurrent.ExecutionContext
import scala.scalajs.js

object global {
  /** Exports the given properties into window.name = value */
  def export(properties: (String, js.Any)*): Unit = {
    properties.foreach { case (key, value) =>
      js.Object.defineProperty(window, key, js.Dynamic.literal(
        configurable = true,
        enumerable = true,
        value = value,
        writable = true
      ).asInstanceOf[js.PropertyDescriptor])
    }
  }

  trait GlobalContext {
    implicit val ec: ExecutionContext = ExecutionContext.global
  }

}
