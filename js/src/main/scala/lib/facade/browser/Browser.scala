package lib.facade.browser

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** This is a package that attempts to detect a browser vendor and version
  * https://www.npmjs.com/package/detect-browser
  */
@js.native
@JSGlobal("Browser")
object Browser extends js.Object {
  /** Returns the name of the browser */
  val name: String = js.native
}