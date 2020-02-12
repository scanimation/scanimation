package lib

import lib.facade.pixi.{BaseTexture, Graphics, Loader, Texture}
import org.scalajs.dom.raw.HTMLImageElement
import scanimation.common._
import scanimation.util.logging.Logging

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js.|
import scala.util.Try
import scala.util.control.NonFatal

/** The HTML5 Creation Engine
  * https://www.pixijs.com/ */
object pixi extends Logging {
  override protected def logKey: String = "pixi"

  //  /** Creates pixi application contained in given drawing box */
  //  def create(box: DrawingBox): Application = {
  //    val app = new Application(js.Dynamic.literal(
  //      width = box.layout.relBounds().size.x.toInt max 1,
  //      height = box.layout.relBounds().size.y.toInt max 1,
  //      antialias = true,
  //      resolution = 1,
  //      transparent = true
  //    ))
  //    val canvas = app.view.asInstanceOf[HTMLCanvasElement]
  //    box.layout.relBounds /> {
  //      case bounds => app.renderer.resize(bounds.size.x max 1, bounds.size.y max 1)
  //    }
  //    box.registerCanvas(canvas)
  //    app
  //  }

  /** Sync object for loading assets */
  private val loaderSync = new Object()
  /** Contains a map of mutexes per loader to allow fake async loading logic */
  private var loaderMutex: Map[Loader, Future[Unit]] = Map()

  implicit class LoaderOps(val loader: Loader) extends AnyVal {
    /** Loads the given asset asynchronously */
    def loadAsync(name: String, asset: String | HTMLImageElement)(implicit ec: ExecutionContext): Future[BaseTexture] = loaderSync.synchronized {
      val promise = Promise[BaseTexture]()
      val mutex = loaderMutex.getOrElse(loader, UnitFuture)
      mutex.onComplete { _ =>
        try {
          log.info(s"loading asset [$name]")
          loader
            .reset()
            .add(asset)
            .load(() => {
              log.info(s"successfully loaded asset [$name]")
              promise.success(Texture.from(asset))
            })
          loader.onError.add(() => {
            log.warn(s"failed to load asset [$name]")
            promise.failure(new IllegalStateException(s"failed to load asset [$name]"))
          })
        } catch {
          case NonFatal(up) =>
            log.error(s"failed to load asset [$name]", up)
            promise.failure(new IllegalStateException(s"failed to load asset [$name]"))
        }
      }
      loaderMutex = loaderMutex + (loader -> promise.future.clear)
      promise.future
    }
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