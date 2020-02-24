package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.Sprite")
class Sprite extends Container {
  def this(texture: Texture = js.native) = this()

  var texture: Texture = js.native
}