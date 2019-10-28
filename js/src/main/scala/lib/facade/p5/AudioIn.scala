package lib.facade.p5

import org.scalajs.dom.experimental.mediastream.MediaStream

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** Get audio from an input, i.e. your computer's microphone.
  * https://p5js.org/reference/#/p5.AudioIn */
@js.native
@JSGlobal("p5.AudioIn")
class AudioIn extends js.Object {
  /** Creates a microphone input requesting access in the browser
    *
    * @param errorCallback a function to call if there is an error accessing the AudioIn. For example, Safari and iOS devices do not currently allow microphone access. (Optional)
    */
  def this(errorCallback: js.Function0[Unit] = js.native) = this()

  /** Client must allow browser to access their microphone / audioin source. Default: false. Will become true when the client enables access. */
  val enabled: Boolean = js.native

  /** Returns the microphone stream reference */
  val stream: MediaStream = js.native

  /** Start processing audio input.
    * This enables the use of other AudioIn methods like getLevel().
    * Note that by default, AudioIn is not connected to p5.sound's output.
    * So you won't hear anything unless you use the connect() method.
    *
    * @param successCallback name of a function to call on success
    * @param errorCallback   name of a function to call if there was an error; for example, some browsers do not support getUserMedia
    */
  def start(successCallback: js.Function0[Unit] = js.native, errorCallback: js.Function0[Unit] = js.native): Unit = js.native

  /** Turn the AudioIn off.
    * If the AudioIn is stopped, it cannot getLevel().
    * If re-starting, the user may be prompted for permission access.
    */
  def stop(): Unit = js.native

  /** Read the Amplitude (volume level) of an AudioIn.
    * The AudioIn class contains its own instance of the Amplitude class to help make it easy to get a microphone's volume level.
    * NOTE: AudioIn must .start() before using .getLevel().
    *
    * @param smoothing optional smoothing value (0.0 < 1.0)
    */
  def getLevel(smoothing: Double = js.native): Double = js.native
}