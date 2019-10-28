package lib.facade.p5

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** FFT (Fast Fourier Transform) is an analysis algorithm that isolates individual audio frequencies within a waveform.
  * https://p5js.org/reference/#/p5.FFT
  */
@js.native
@JSGlobal("p5.AudioIn")
class FFT extends js.Object {
  /** Creates FFT for analysis
    *
    * @param smoothing smooth results of Freq Spectrum; 0.0 < smoothing < 1.0; defaults to 0.8
    * @param bins      length of resulting array; must be a power of two between 16 and 1024; defaults to 1024.
    */
  def this(smoothing: Double = js.native, bins: Int = js.native) = this()

  /** Returns an array of amplitude values (between 0 and 255) across the frequency spectrum.
    * Length is equal to FFT bins (1024 by default).
    * The array indices correspond to frequencies (i.e. pitches), from the lowest to the highest that humans can hear.
    * Each value represents amplitude at that slice of the frequency spectrum.
    * Must be called prior to using getEnergy()
    */
  def analyze(): js.Array[Double] = js.native
}