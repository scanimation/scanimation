package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.Container")
class Container extends DisplayObject {
  var children: js.Array[DisplayObject] = js.native

  def addChild(child: DisplayObject): DisplayObject = js.native

  def removeChild(child: DisplayObject): DisplayObject = js.native
}
