package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.loaders.Loader")
class Loader extends js.Object {
  val progress: Double = js.native

  def add(path: String): Loader = js.native

  def add(paths: js.Array[String]): Loader = js.native

  def load(callback: js.Function0[Unit]): Loader = js.native

  def resources: lib.facade.pixi.resources.type = js.native

  //  def on(eventType: EventType, handler: js.Function2[Loader, Resource, Unit]): Loader = js.native

  def on(eventType: String, handler: js.Function2[Loader, Resource, Unit]): Loader = js.native

  def reset(): Loader = js.native
}