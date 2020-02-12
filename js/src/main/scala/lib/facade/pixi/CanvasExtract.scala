package lib.facade.pixi

import org.scalajs.dom.raw.Blob

import scala.scalajs.js

/** https://www.html5gamedevs.com/topic/31190-saving-pixi-content-to-image/ */
@js.native
class CanvasExtract extends js.Object {
  def toBlob(readyCode: js.Function1[Blob, Unit], mime: String): CanvasExtract = js.native
}