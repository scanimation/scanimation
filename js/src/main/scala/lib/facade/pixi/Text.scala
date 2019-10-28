package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.Text")
class Text extends DisplayObject {

  def this(text: String, style: TextStyle) = this()

  var text: String = js.native
  var style: TextStyle = js.native

}