package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.ticker.Ticker")
class Ticker extends js.Object {
  def add(code: js.Function1[Double, Unit]): Ticker = js.native
}