package lib.facade.pixi

import scala.scalajs.js

/** https://www.html5gamedevs.com/topic/31190-saving-pixi-content-to-image/ */
@js.native
class Extract extends js.Object {
  def canvas(displayObject: DisplayObject): CanvasExtract = js.native
}