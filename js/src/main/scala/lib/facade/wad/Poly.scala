package lib.facade.wad

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** PolyWads can detect the frequency of their input. */
@js.native
@JSGlobal("Wad.Poly")
class Poly extends Wad {
  /** Turns on calculation of the pitch and note name of wad input 60 times per second. These values are stored in wad.pitch and wad.noteName */
  def updatePitch(): Poly = js.native

  /** Stops the pitch detection */
  def stopUpdatingPitch(): Poly = js.native

  /** Contains the detected pitch */
  var pitch: js.UndefOr[Double] = js.native

  /** Contains the detected note label */
  var noteName: js.UndefOr[String] = js.native
}