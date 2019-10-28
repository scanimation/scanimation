package lib.facade.ml5

import org.scalajs.dom.AudioContext
import org.scalajs.dom.experimental.mediastream.MediaStream

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** Friendly Machine Learning for the Web
  * https://ml5js.org/
  */
@js.native
@JSGlobal("ml5")
object ml5 extends js.Object {
  /** A pitch detection algorithm is a way of estimating the pitch or fundamental frequency of an audio signal.
    * This method allows to use a pre-trained machine learning pitch detection model to estimate the pitch of sound file.
    *
    * At present ml5.js only supports the CREPE model.
    * This model is a direct port of github.com/marl/crepe and only works with direct input from the browser microphone.
    *
    * https://ml5js.github.io/ml5-library/docs/#/reference/pitch-detection
    */
  def pitchDetection(model: String, audioContext: AudioContext, stream: MediaStream, modelLoaded: js.Function0[Unit]): PitchDetection = js.native
}