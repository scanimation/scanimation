package scanimation

import org.scalajs.dom
import scanimation.common.Transition.Missing
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

    /** Launches the controller at a given application path */
    def start(path: String): Future[Unit] = for {
      _ <- UnitFuture
      _ = log.info(s"starting at path [$path]")

      _ = router.start(this)
      _ <- pages.pages.start(this)
      _ = timer.start(60, () => model.tick.write(model.tick() + 1))
      _ = animator.start(() => model.frame.write(model.frame() + 1))
      _ = log.info(s"started")

      _ = model.page /> {
        case page => http.updateTitle(s"Scanimation - ${page.title}")
      }
      _ = bindFrames()
      _ = log.info("bound")
    } yield ()

    /** Binds the frame list listeners */
    def bindFrames(): Unit = {
      implicit val framesListenerId: ListenerId = ListenerId()
      model.frames.onAdd { case (id, frame) =>
      }
      model.frames.onRemove { case (id, frame) =>
      }
      model.frames.data /> {
        case Nil =>
          model.frameSize.write(None)
      }
      (model.frames.data && model.frameOverlap) /> {
        case (frames, Some(overlap)) if frames.nonEmpty && frames.size % overlap == 0 =>
          model.frameCount.write(Some(frames.size / overlap))
        case _ =>
          model.frameCount.write(None)
      }
    }

    /** Redirect to given page within scanimation */
    def showPage(page: Page): Unit = {
      model.page.write(page)
    }

    /** Opens up files upload dialogue to add frame images */
    def addFrames(frames: List[Frame]): Unit = {
      log.info("adding frames")
      model.frames.addList(frames)
    }

    /** Removes all of the frames from frames list */
    def clearFrames(): Unit = {
      log.info("clearing frames")
      model.frames.clear()
      model.scanimation.reset
    }

    /** Displays the input animation in the preview */
    def showAnimation(): Unit = {
      log.info("showing animation")
    }

    /** Updates the frame width setting to a given value */
    def setFrameWidth(value: Option[Int]): Unit = {
      log.info(s"setting frame width to [$value]")
      model.frameWidth.write(value)
    }

    /** Updates the frame overlap setting to a given value */
    def setFrameOverlap(value: Option[Int]): Unit = {
      log.info(s"setting frame overlap to [$value]")
      model.frameOverlap.write(value)
    }

    /** Updates the scanimation direction setting to a given value */
    def setDirection(value: Directions.Value): Unit = {
      log.info(s"setting direction to [$value]")
      model.direction.write(value)
    }

    /** Replaces all modified settings with recommended values */
    def resetSettings(): Unit = {
      log.info("resetting settings to default")
      model.direction.write(Default.direction)
      model.frameWidth.write(Some(Default.frameWidth))
      model.frameOverlap.write(Some(Default.frameOverlap))
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

  /** Default values for scanimation settings */
  object Default {
    val frameWidth = 9
    val frameOverlap = 1
    val direction: Directions.Value = Directions.Left
  }

  /** Defines model with common fields
    *
    * @param tick         the current update tick
    * @param frame        the current rendering frame
    * @param page         currently displayed scanimation page
    * @param frames       a list of frame images uploaded by the user
    * @param frameSize    the image resolution of the first loaded frame
    * @param frameCount   the number of scanimation frames after overlap
    * @param frameWidth   the width of the gap between grid lines in pixels
    * @param frameOverlap the number of animation frames put into single scanimation frame
    * @param scanimation  the scanimation computing results
    */
  case class Model(tick: Writeable[Long] = Data(0),
                   frame: Writeable[Long] = Data(0),
                   page: Writeable[Page] = LazyData(router.parsePage),

                   frames: ListData[Frame] = ListData(),
                   frameSize: Writeable[Option[Vec2i]] = Data(None),
                   frameCount: Writeable[Option[Int]] = Data(None),
                   frameWidth: Writeable[Option[Int]] = Data(Some(Default.frameWidth)),
                   frameOverlap: Writeable[Option[Int]] = Data(Some(Default.frameOverlap)),
                   direction: Writeable[Directions.Value] = Data(Default.direction),
                   scanimation: TransitionData[CompleteScanimation] = Data(Missing()))

  /** Defines the single animation frame
    *
    * @param name    the name of the imported file
    * @param size    the size of the imported image
    * @param content the image content
    */
  case class Frame(name: String, size: Vec2i, content: String)

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

  /** Describes the direction where scanimation grid moves to */
  object Directions extends Enumeration {
    val Left, Right, Up, Down = Value
  }

}