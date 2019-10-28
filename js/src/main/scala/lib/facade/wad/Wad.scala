package lib.facade.wad

import org.scalajs.dom.raw.AudioContext

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** Web Audio DAW. Use the Web Audio API for dynamic sound synthesis. It's like jQuery for your ears.
  * https://github.com/rserota/wad */
@js.native
@JSGlobal("Wad")
class Wad extends js.Object {
  def this(config: js.Dynamic = js.native) = this()

  /** Change the volume of a Wad at any time, including during playback */
  def setVolume(level: Double): Wad = js.native

  /** Attaches the given wad as the source for current one */
  def add(source: Wad): Wad = js.native

  /** Turns on the playback of the wad */
  def play(): Wad = js.native

  /** Turns off the playback of the wad */
  def stop(): Wad = js.native

  /** The raw audio context for the wad */
  val audioContext: AudioContext = js.native
}

object Wad extends js.Object {
  /** Returns true if mic consent has been received */
  var micConsent: Boolean = js.native
}