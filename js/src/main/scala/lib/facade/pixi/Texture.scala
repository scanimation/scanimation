package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.Texture")
object Texture extends js.Object {
  /** Helper function that creates a new Texture based on the source you provide.
    * The source can be - frame id, image url, video url, canvas element, video element, base texture */
  def from(path: String): BaseTexture = js.native
}