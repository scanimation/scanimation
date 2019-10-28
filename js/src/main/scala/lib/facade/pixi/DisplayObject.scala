package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.DisplayObject")
class DisplayObject extends js.Object {
  var uuid: String = js.native

  var x: Double = js.native
  var y: Double = js.native
  var z: Double = js.native
  var position: Point = js.native

  var width: Double = js.native
  var height: Double = js.native
  var scale: Point = js.native
  var skew: Point = js.native

  var rotation: Double = js.native
  var anchor: Point = js.native
  var pivot: Point = js.native

  /** Current transform of the object based on local factors: position, scale, other stuff */
  val localTransform: Matrix = js.native
  /** Current transform of the object based on world (parent) factors **/
  val worldTransform: Matrix = js.native

  var mask: DisplayObject = js.native
  var alpha: Double = js.native
  var visible: Boolean = js.native

  val parent: Container = js.native

  var interactive: Boolean = js.native
  var buttonMode: Boolean = js.native
  var hitArea: Rectangle = js.native

  var filters: js.Array[Filter] = js.native
  var tint: Double = js.native

  def getGlobalPosition(): Point = js.native

  def toGlobal(point: Point): Point = js.native

  def toLocal(point: Point, other: DisplayObject): Point = js.native

  def on(eventType: String, handler: js.Function0[Unit]): this.type = js.native

  def setTransform(x: Double, y: Double, scaleX: Double, scaleY: Double, rotation: Double, skewX: Double, skewY: Double, pivotX: Double = js.native, pivotY: Double = js.native): this.type = js.native
}