package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.Matrix")
class Matrix extends js.Object {
  /** Scale x */
  var a: Double = js.native
  /** Unknown */
  var b: Double = js.native
  /** Unknown */
  var c: Double = js.native
  /** Scale y */
  var d: Double = js.native
  /** Position x */
  var tx: Double = js.native
  /** Position Y */
  var ty: Double = js.native

  /** Changes the values of the given matrix to be the same as the ones in this matrix */
  def copy(matrix: Matrix): Matrix = js.native
}