package lib.facade.history

import org.scalajs.dom.raw.Location

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** https://github.com/ReactTraining/history/blob/3f69f9e07b0a739419704cffc3b3563133281548/docs/GettingStarted.md */
@js.native
@JSGlobal("History")
object History extends js.Object {
  /** Get the current location */
  val location: Location = js.native

  /** Listen for changes to the current location */
  def listen(listener: js.Function2[Location, String, Unit]): js.Function0[Unit] = js.native

  /** Use push, replace, and go to navigate around */
  def push(location: String, data: js.Dynamic = js.Dynamic.literal()): Unit = js.native
}