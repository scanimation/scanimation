package lib

import lib.facade.wad._
import scanimation.model._

import scala.concurrent.duration._
import scala.scalajs.js

/** Scala api for Web Audio DAW library.
  * https://github.com/rserota/wad */
object wad {

  /** Configures the wad
    *
    * @param source the audio file to load or which generator to use
    * @param volume peak volume can range from 0 to an arbitrarily high number, but you probably shouldn't set it higher than 1
    * @param loop   if true, the audio will loop; this parameter only works for audio clips, and does nothing for oscillators
    * @param rate   how fast to play an audio clip, relative to its normal speed: 2.0 is double speed, 0.5 is half speed, etc
    * @param offset where in the audio clip playback begins, measured from the start of the audio clip
    * @param pitch  set a default pitch on the constructor if you don't want to set the pitch on play()
    * @param detune set a default detune on the constructor if you don't want to set detune on play(); detune is measured in cents; 100 cents is equal to 1 semitone
    */
  case class Config(source: Source,
                    volume: Double = 1.0,
                    loop: Boolean = false,
                    rate: Double = 1.0,
                    offset: FiniteDuration = 0.seconds,
                    pitch: Note = A4,
                    detune: Int = 0)

  /** Describes the source stream for the wad
    *
    * @param value a set value for generators or the url to audio file
    */
  case class Source(value: String)

  /** Use microphone input as the wad source, will request user to accept the input */
  val Mic = Source("mic")
  /** Use the sine wave generator */
  val Sine = Source("sine")
  /** Use the square wave generator */
  val Square = Source("square")
  /** Use the sawtooth wave generator */
  val Sawtooth = Source("sawtooth")
  /** Use the triangle wave generator */
  val Triangle = Source("triangle")

  /** Creates a wad */
  def apply(config: Config): Wad = new Wad(js.Dynamic.literal(
    source = config.source.value,
    volume = config.volume,
    loop = config.loop,
    rate = config.rate,
    offset = config.offset.toMillis / 1000.0,
    pitch = config.pitch.label,
    detune = config.detune
  ))

  /** Creates a poly wad */
  def poly(): Poly = new Poly()

  /** Returns true if mic consent has been received */
  def micConsent: Boolean = Wad.micConsent
}