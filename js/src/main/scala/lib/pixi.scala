package lib

import lib.facade.pixi.{Application, Graphics}
import scanimation.box._
import scanimation.common.{Color, Colors, Vec2d}
import scanimation.util.logging.Logging
import org.scalajs.dom.raw.HTMLCanvasElement

import scala.scalajs.js

/** The HTML5 Creation Engine
  * https://www.pixijs.com/ */
object pixi extends Logging {
  override protected def logKey: String = "pixi"

  /** Creates pixi application contained in given drawing box */
  def create(box: DrawingBox): Application = {
    val app = new Application(js.Dynamic.literal(
      width = box.layout.relBounds().size.x.toInt max 1,
      height = box.layout.relBounds().size.y.toInt max 1,
      antialias = true,
      resolution = 1,
      transparent = true
    ))
    val canvas = app.view.asInstanceOf[HTMLCanvasElement]
    box.layout.relBounds /> {
      case bounds => app.renderer.resize(bounds.size.x max 1, bounds.size.y max 1)
    }
    box.registerCanvas(canvas)
    app
  }

  implicit class GraphicsOps(val graphics: Graphics) extends AnyVal {
    /** Draws a rectangle with given size */
    def fillRect(size: Vec2d, position: Vec2d = Vec2d.Zero, color: Color = Colors.PureBlack): Graphics = {
      val (x, y, w, h) = (position.x, position.y, size.x, size.y)
      graphics
        .beginFill(color.toDouble)
        .moveTo(x, y)
        .lineTo(x + w, y)
        .lineTo(x + w, y + h)
        .lineTo(x, y + h)
        .lineTo(x, y)
        .endFill()
    }

    /** Draws a rectangle with rounded corners */
    def fillRoundRect(size: Vec2d, radius: Double, color: Color = Colors.PureBlack): Graphics = {
      val (w, h, r) = (size.x, size.y, radius)
      graphics
        .beginFill(color.toDouble)
        .moveTo(r, 0)
        .lineTo(w - r, 0).quadraticCurveTo(w, 0, w, r)
        .lineTo(w, h - r).quadraticCurveTo(w, h, w - r, h)
        .lineTo(r, h).quadraticCurveTo(0, h, 0, h - r)
        .lineTo(0, r).quadraticCurveTo(0, 0, r, 0)
        .endFill()
    }

    /** Draws a rectangle with cut corners */
    def fillCutRect(size: Vec2d, cut: Double, color: Color = Colors.PureBlack): Graphics = {
      val (w, h, c) = (size.x, size.y, cut)
      graphics
        .beginFill(color.toDouble)
        .moveTo(c, 0)
        .lineTo(w - c, 0)
        .lineTo(w, c)
        .lineTo(w, h - c)
        .lineTo(w - c, h)
        .lineTo(c, h)
        .lineTo(0, h - c)
        .lineTo(0, c)
        .lineTo(c, 0)
        .endFill()
    }
  }

}