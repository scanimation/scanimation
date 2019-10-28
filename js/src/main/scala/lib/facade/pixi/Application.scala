package lib.facade.pixi

import org.scalajs.dom.raw.Node

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.Application")
class Application extends js.Object {
  def this(config: js.Dynamic = js.native) = this()

  val loader: Loader = js.native
  val renderer: SystemRenderer = js.native
  val view: Node = js.native
  val stage: Container = js.native
  val ticker: Ticker = js.native
}