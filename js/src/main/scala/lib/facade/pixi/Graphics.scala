package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.Graphics")
class Graphics extends DisplayObject {

  def lineStyle(width: Double = js.native, color: Double = js.native, alpha: Double = js.native, alignment: Double = js.native): Graphics = js.native

  def clear(): Graphics = js.native

  def beginFill(color: Double = js.native, alpha: Double = js.native): Graphics = js.native

  def moveTo(x: Double, y: Double): Graphics = js.native

  def lineTo(x: Double, y: Double): Graphics = js.native

  def quadraticCurveTo(cpx: Double, cpy: Double, x: Double, y: Double): Graphics = js.native

  def endFill(): Graphics = js.native

}