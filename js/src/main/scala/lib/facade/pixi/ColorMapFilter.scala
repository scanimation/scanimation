package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.filters.ColorMapFilter")
class ColorMapFilter extends Filter {
  def this(colorMap: BaseTexture, nearest: Boolean = js.native) = this()
}