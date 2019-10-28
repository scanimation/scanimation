package lib.facade.ffo

import scala.scalajs.js
import scala.scalajs.js.Promise
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("FontFaceObserver")
class FontFaceObserver extends js.Object {
  def this(family: String = js.native) = this()

  def load(): Promise[js.Object] = js.native
}