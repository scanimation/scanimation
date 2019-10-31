package scanimation

import org.scalajs.dom
import scanimation.common.Transition.{Loaded, Missing}
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
      model.frames /> {
        case Nil => model.selectedFrame.write(None)
      }
      log.info("bound")
    }

    /** Redirect to given page within scanimation */
    def showPage(page: Page): Unit = {
      model.page.write(page)
    }

    /** Opens up files upload dialogue to add frame images */
    def addFrames(): Unit = {
      log.info("adding frames")
      val frames = (0 until 6).toList.map { index =>
        Loaded(0, 0, Frame(s"frame${index + 1}.png", 1920 xy 1080, s"foo$index"))
      }
      model.frames.write(frames)
    }

    /** Removes all of the frames from frames list */
    def clearFrames(): Unit = {
      log.info("clearing frames")
      model.frames.write(Nil)
      model.scanimation.reset
    }

    /** Displays the input animation in the preview */
    def showAnimation(): Unit = {
      log.info("showing animation")
    }

    /** Marks the frame with given index for the preview */
    def selectFrame(index: Int): Unit = {
      log.info(s"selecting frame [$index]")
      model.selectedFrame.write(Some(index))
    }

    /** Attempts to move the frame with given index up */
    def moveFrameUp(index: Int): Unit = {
      log.info(s"moving frame [$index] up")
      val current = model.frames()
      model.frames.write(current.take(index - 1) ++ current.lift(index) ++ current.lift(index - 1) ++ current.drop(index + 1))
      model.selectedFrame.write(Some(index - 1))
    }

    /** Attempts to move the frame with given index down */
    def moveFrameDown(index: Int): Unit = {
      log.info(s"moving frame [$index] down")
      val current = model.frames()
      model.frames.write(current.take(index) ++ current.lift(index + 1) ++ current.lift(index) ++ current.drop(index + 2))
      model.selectedFrame.write(Some(index + 1))
    }

    /** Removes the frame with given index from frames list */
    def deleteFrame(index: Int): Unit = {
      log.info(s"deleting frame [$index]")
      val current = model.frames()
      model.frames.write(current.take(index) ++ current.drop(index + 1))
      model.selectedFrame.write(None)
    }

    /** Replaces all modified settings with recommended values */
    def resetSettings(): Unit = {
      log.info("resetting settings to default")
    }

    /** Requests the server to produce the scanimation */
    def computeScanimation(): Unit = {
      log.info("computing scanimation")
      model.scanimation.loading
      dom.window.setTimeout({ () =>
        model.scanimation.loaded(CompleteScanimation("foo", "bar"))
      }, 5000)
    }

    /** Plays the computed scanimation in preview section */
    def showScanimation(): Unit = {
      log.info("showing scanimation")
    }

    /** Downloads the produced scanimation image */
    def exportScanimation(): Unit = {
      log.info("exporting scanimation")
    }

    /** Downloads the produced scanimation grid image */
    def exportGrid(): Unit = {
      log.info("exporting scanimation grid")
    }
  }

  /** Defines model with common fields
    *
    * @param tick          the current update tick
    * @param frame         the current rendering frame
    * @param screen        the current screen size
    * @param scale         the current screen scale
    * @param mouse         current mouse coordinates
    * @param page          currently displayed scanimation page
    * @param frames        a list of frame images uploaded by the user
    * @param selectedFrame the index of the frame selected for the preview
    * @param scanimation   the scanimation computing results
    */
  case class Model(tick: Writeable[Long] = Data(0),
                   frame: Writeable[Long] = Data(0),
                   screen: Writeable[Vec2i] = Data(0 xy 0),
                   scale: Writeable[Double] = Data(1.0),
                   mouse: Writeable[Vec2d] = Data(Vec2d.Zero),
                   page: Writeable[Page] = LazyData(router.parsePage),

                   frames: Writeable[List[Transition[Frame]]] = LazyData(Nil),
                   selectedFrame: Writeable[Option[Int]] = LazyData(None),
                   scanimation: TransitionData[CompleteScanimation] = Data(Missing()))

  /** Defines the single animation frame
    *
    * @param name the name of the uploaded file
    * @param size the size of the uploaded image
    * @param url  the url of the frame image uploaded on the server
    */
  case class Frame(name: String, size: Vec2i, url: String)

  /** Contains scanimation processing results
    *
    * @param scanimation the image url of combined animation frames
    * @param grid        the image url of the scanimation grid with transparent background
    */
  case class CompleteScanimation(scanimation: String, grid: String)

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