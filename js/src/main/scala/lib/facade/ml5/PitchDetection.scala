package lib.facade.ml5

import scala.scalajs.js
import scala.scalajs.js.UndefOr

/** https://ml5js.github.io/ml5-library/docs/#/reference/pitch-detection */
class PitchDetection extends js.Object {
  /** Current frequency */
  var frequency: UndefOr[Double] = js.native

  /** Gets the pitch
    *
    * @param callback will be called once the pitch is detected
    */
  def getPitch(callback: js.Function2[UndefOr[Double], UndefOr[Double], Unit]): Unit = js.native
}