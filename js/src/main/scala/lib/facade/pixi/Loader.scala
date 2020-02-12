package lib.facade.pixi

import org.scalajs.dom.raw.HTMLImageElement

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.|

@js.native
@JSGlobal("PIXI.loaders.Loader")
class Loader extends js.Object {
  val progress: Double = js.native

  def add(path: String | HTMLImageElement): Loader = js.native

  def add(paths: js.Array[String | HTMLImageElement]): Loader = js.native

  def load(callback: js.Function0[Unit]): Loader = js.native

  def resources: lib.facade.pixi.resources.type = js.native

  /** Listener that is called once per errored file */
  def onError: LoaderListener = js.native

  def reset(): Loader = js.native
}