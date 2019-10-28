package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.filters.ColorMatrixFilter")
class ColorMatrixFilter extends Filter {
  var matrix: js.Array[Double] = js.native
}