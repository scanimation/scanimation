package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.Sprite")
class Sprite extends Container {
  def this(texture: BaseTexture = js.native) = this()

  var texture: BaseTexture = js.native
}