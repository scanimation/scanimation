package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.Rectangle")
class Rectangle extends js.Object {
  def this(x: Double, y: Double, width: Double, height: Double) = this()

  var x: Double = js.native
  var y: Double = js.native
  var width: Double = js.native
  var height: Double = js.native
}