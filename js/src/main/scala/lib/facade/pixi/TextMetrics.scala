package lib.facade.pixi

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("PIXI.TextMetrics")
class TextMetrics extends js.Object {
  /** The text that was measured */
  val text: String = js.native

  /** The style that was measured */
  val style: TextStyle = js.native

  /** The measured width of the text */
  val width: Double = js.native

  /** The measured height of the text */
  val height: Double = js.native

  /** An array of the lines of text broken by new lines and wrapping if specified in style */
  val lines: js.Array[String] = js.native

  /** An array of the line widths for each line matched to lines */
  val lineWidths: js.Array[Double] = js.native

  /** The measured line height for this style */
  val lineHeight: Double = js.native

  /** The maximum line width for all measured lines */
  val maxLineWidth: Double = js.native

  /** The font properties object from TextMetrics.measureFont */
  val fontProperties: js.Object = js.native
}

@js.native
@JSGlobal("PIXI.TextMetrics")
object TextMetrics extends js.Object {
  /** Measures the supplied string of text and returns a Rectangle */
  def measureText(text: String, style: TextStyle, wordWrap: Boolean = js.native): TextMetrics = js.native
}