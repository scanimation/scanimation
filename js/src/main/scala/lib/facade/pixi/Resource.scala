package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.loaders.Resource")
class Resource extends js.Object {
  def texture: BaseTexture = js.native

  def textures: Textures = js.native

  def url: String = js.native
}