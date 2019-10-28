package lib

import lib.facade.history.{History => h}
import org.scalajs.dom.raw.Location

/** The history library lets you easily manage session history anywhere JavaScript runs.
  * https://www.npmjs.com/package/history */
object history {
  /** Starts a listener for history events */
  def start(listener: Location => Unit): Unit = {
    h.listen((location, action) => listener.apply(location))
  }

  /** Returns the current location */
  def location: Location = h.location

  /** Replaces current page location with given one */
  def push(location: String): Unit = h.push(location)
}