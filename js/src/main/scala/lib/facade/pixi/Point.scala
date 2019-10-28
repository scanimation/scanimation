package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.Point")
class Point extends js.Object {
  var x: Double = js.native
  var y: Double = js.native

  def set(x: Double, y: Double): Unit = js.native
}