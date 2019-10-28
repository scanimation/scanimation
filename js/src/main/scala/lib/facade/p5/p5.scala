package lib.facade.p5

import org.scalajs.dom.raw.AudioContext

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** https://github.com/processing/p5.js/blob/3abbbfbe8c95dc5dda6244c01baa0cbdd8e90697/src/dom/dom.js */
@js.native
@JSGlobal("p5.prototype")
object p5 extends js.Object {
  /** Root audio context. Useful for users who would like to dig deeper into the Web Audio API. */
  def getAudioContext(): AudioContext = js.native
}