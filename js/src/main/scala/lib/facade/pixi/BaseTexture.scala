package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.BaseTexture")
class BaseTexture extends Texture {
  var frame: Rectangle = js.native
}