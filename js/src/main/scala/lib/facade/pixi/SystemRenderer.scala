package lib.facade.pixi

import org.scalajs.dom.raw.HTMLCanvasElement

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.SystemRenderer")
class SystemRenderer extends js.Object {
  var autoResize: Boolean = js.native

  var view: HTMLCanvasElement = js.native

  var backgroundColor: Double = js.native

  val plugins: RendererPlugins = js.native

  def resize(width: Double, height: Double): Unit = js.native
}