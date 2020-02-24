package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.RenderTexture")
class RenderTexture extends Texture

@js.native
@JSGlobal("PIXI.RenderTexture")
object RenderTexture extends js.Object {
  /** A short hand way of creating a render texture */
  def create(width: Int, height: Int): RenderTexture = js.native
}