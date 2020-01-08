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
      bindFrames()
      log.info("bound")
    }

    /** Binds the frame list listeners */
    def bindFrames(): Unit = {
      implicit val framesListenerId: ListenerId = ListenerId()
      model.frames.onAdd { case (id, frame) =>
        frame /> {
          case Loaded(start, end, value) =>
            // TODO: add failure logic if frames are of an incorrect size
            model.frameSize.write(Some(value.size))
        }
      }
      model.frames.onRemove { case (id, frame) =>
        frame.forget()
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
    def addFrames(): Unit = {
      log.info("adding frames")
      val frames: List[Transition[Frame]] = (0 until 10).toList.map { index =>
        Loaded(0, 0, Frame(s"frame${index + 1}.png", 1920 xy 1080, s"foo$index"))
      }
      model.frames.addList(frames.map(f => Data(f)))
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

    /** Replaces all modified settings with recommended values */
    def resetSettings(): Unit = {
      log.info("resetting settings to default")
      model.frameWidth.write(Some(9))
      model.frameOverlap.write(Some(1))
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

                   frames: ListData[TransitionData[Frame]] = ListData(),
                   frameSize: Writeable[Option[Vec2i]] = Data(None),
                   frameCount: Writeable[Option[Int]] = Data(None),
                   frameWidth: Writeable[Option[Int]] = Data(Some(9)),
                   frameOverlap: Writeable[Option[Int]] = Data(Some(1)),
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