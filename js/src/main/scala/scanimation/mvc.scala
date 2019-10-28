package scanimation

import scanimation.common._
import scanimation.conf.ScanimationConfig
import scanimation.model.Note
import scanimation.util.global.GlobalContext
import scanimation.util.http
import scanimation.util.logging.Logging
import scanimation.util.timer.{Animator, Timer}

import scala.concurrent.Future

object mvc {

  /** Defines controller with common functionality */
  case class Controller(model: Model, config: ScanimationConfig) extends Logging with GlobalContext {
    override protected def logKey: String = "controller"

    val timer = new Timer()
    val animator = new Animator()

    /** Updates the rendering screen size */
    def setScreenSize(size: Vec2i): Unit = model.screen.write(size)

    /** Updates the global mouse position on the screen */
    def setMousePosition(mouse: Vec2d): Unit = model.mouse.write(mouse)

    /** Launches the controller at a given application path */
    def start(path: String): Future[Unit] = Future {
      log.info(s"starting at path [$path]")
      router.start(this)
      pages.pages.start(this)
      timer.start(60, () => model.tick.write(model.tick() + 1))
      animator.start(() => model.frame.write(model.frame() + 1))
      log.info(s"started")

      model.page /> {
        case page => http.updateTitle(s"Scanimation - ${page.title}")
      }
      log.info("bound")
    }

    /** Redirect to given page within scanimation */
    def showPage(page: Page): Unit = {
      model.page.write(page)
    }
  }

  /** Defines model with common fields
    *
    * @param tick         the current update tick
    * @param frame        the current rendering frame
    * @param screen       the current screen size
    * @param scale        the current screen scale
    * @param mouse        current mouse coordinates
    * @param page         currently displayed scanimation page
    */
  case class Model(tick: Writeable[Long] = Data(0),
                   frame: Writeable[Long] = Data(0),
                   screen: Writeable[Vec2i] = Data(0 xy 0),
                   scale: Writeable[Double] = Data(1.0),
                   mouse: Writeable[Vec2d] = Data(Vec2d.Zero),
                   page: Writeable[Page] = LazyData(router.parsePage))

  /** The current application page */
  sealed trait Page {
    /** Returns the subtitle for the page */
    def title: String

    /** Returns true if the page required microphone input */
    def detection: Boolean = false
  }

  /** The starting page of the application */
  case class BuilderPage() extends Page {
    override def title: String = "Builder"
  }

  /** Currently detected note from the spectrum
    *
    * @param note      the closest note to detected pitch
    * @param frequency the original detected pitch
    * @param cents     the cents distance between closest note and detected pitch
    */
  case class Detection(note: Note,
                       frequency: Double,
                       cents: Double)

}