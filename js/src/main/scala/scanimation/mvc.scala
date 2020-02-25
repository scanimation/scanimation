package scanimation

import lib.facade.pixi.{Application, Graphics, RenderTexture, Texture}
import scanimation.common.Transition.{Missing, TransitionException}
import scanimation.common._
import scanimation.conf.ScanimationConfig
import scanimation.model.Note
import scanimation.ops._
import lib.pixi._
import scanimation.util.global.GlobalContext
import scanimation.util.http
import scanimation.util.logging.Logging
import scanimation.util.timer.{Animator, Timer}

import scala.concurrent.Future
import scala.scalajs.js.Dynamic

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
      _ = bindSettings()
      _ = bindScanimate()
      _ = bindLogging()
      _ = log.info("bound")
    } yield ()

    /** Binds the model value loggers */
    def bindLogging(): Unit = {
      model.canScanimate /> { case value => log.info(s"can scanimate [$value]") }
    }

    /** Binds the frame list listeners */
    def bindFrames(): Unit = {
      model.frames.data /> {
        case head :: tail =>
          model.imageSize.write(Some(head.size))
        case Nil =>
          model.imageSize.write(None)
      }
      (model.frames.data && model.frameOverlap) /> {
        case (frames, Some(overlap)) if frames.nonEmpty && frames.size % overlap == 0 =>
          model.frameCount.write(Some(frames.size / overlap))
        case _ =>
          model.frameCount.write(None)
      }
      model.frames.data /> { case _ => clearScanimation() }
    }

    /** Binds the settings listeners */
    def bindSettings(): Unit = {
      (model.frameWidth && model.frameOverlap && model.direction) /> {
        case ((Some(width), Some(overlap)), direction) if width == Default.frameWidth && overlap == Default.frameOverlap && direction == Default.direction =>
          model.defaultSettings.write(true)
        case _ =>
          model.defaultSettings.write(false)
      }
      (model.frameWidth && model.frameOverlap && model.direction) /> { case _ => clearScanimation() }
    }

    /** Binds the scanimation logic */
    def bindScanimate(): Unit = {
      (model.frameCount && model.imageSize && model.frameWidth && model.frameOverlap) /> {
        case (((Some(count), Some(size)), Some(width)), Some(overlap)) =>
          model.canScanimate.write(true)
        case _ =>
          model.canScanimate.write(false)
      }
    }

    /** Redirect to given page within scanimation */
    def showPage(page: Page): Unit = {
      model.page.write(page)
    }

    /** Opens up files upload dialogue to add frame images */
    def addFrames(frames: List[Frame]): Unit = {
      log.info("adding frames")
      model.frames.addList(frames.sortBy(frame => frame.name))
    }

    /** Deselects all frames and select the frame with given id */
    def selectFrame(id: String): Unit = {
      log.info(s"selecting frame [$id]")
      model.frames.select(id)
    }

    /** Deselects all frames */
    def deselectFrames(): Unit = {
      log.info(s"deselecting frames")
      model.frames.deselectAll()
    }

    /** Shifts the position of the frame up */
    def moveFrameUp(id: String): Unit = {
      log.info(s"moving frame [$id] up")
      model.frames.moveUp(id)
    }

    /** Shifts the position of the frame down */
    def moveFrameDown(id: String): Unit = {
      log.info(s"moving frame [$id] down")
      model.frames.moveDown(id)
    }

    /** Removes the given frame */
    def removeFrame(id: ListElementId): Unit = {
      log.info(s"removing frame [$id]")
      model.frames.remove(id)
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

    /** Runs the scanimation algo */
    def scanimate(): Unit = {
      if (!model.scanimation.isLoading) {
        log.info("computing scanimation")
        scanimateFuture.transition(model.scanimation)
      }
    }

    /** Runs the async scanimation algo */
    def scanimateFuture: Future[CompleteScanimation] = for {
      _ <- UnitFuture
      _ = log.info("reading scanimation parameters")
      frames = model.frames.data()
      size = frames.headOption.map(frame => frame.size).getOrElse(throw ErrorCodes.FramesMissingError)
      _ = frames.find(frame => frame.size != size).foreach(frame => throw ErrorCodes.FrameIncompatibleSizeError(frame, size))
      frameWidth = model.frameWidth().getOrElse(throw ErrorCodes.FrameWidthError)
      overlapCount = model.frameOverlap().getOrElse(throw ErrorCodes.FrameOverlapError)
      direction = model.direction()
      _ = log.info("creating background application")
      app = new Application(Dynamic.literal(
        width = size.x,
        height = size.y,
        antialias = true,
        transparent = false,
        resolution = 1
      ))
      _ = log.info("creating grid graphics")
      gridTexture = RenderTexture.create(size.x, size.y)
      gridContainer = {
        val mask = new Graphics().fillRect(size, Vec2d.Zero, Colors.PureWhite).addTo(app.stage)
        val container = app.stage.sub.maskWith(mask)

        // background
        new Graphics()
          .fillRect(size, Vec2d.Zero, Colors.PureWhite)
          .addTo(container)

        val horizontal = direction == Directions.Left || direction == Directions.Right
        val stripeLength = if (horizontal) size.x else size.y
        val actualCount = frames.size / overlapCount
        val stripeCount = (1 + stripeLength.toDouble / frameWidth / actualCount).toInt
        val stripeWidth = frameWidth * (actualCount - 1)
        (0 until stripeCount).foreach { index =>
          // single stripe
          val stripeSize = if (horizontal) stripeWidth xy size.y else size.x xy stripeWidth
          val stripeOffset = index * frameWidth * actualCount
          val stripePosition = direction match {
            case Directions.Left => stripeOffset xy 0
            case Directions.Right => (size.x - stripeOffset - stripeWidth) xy 0
            case Directions.Up => 0 xy stripeOffset
            case Directions.Down => 0 xy (size.y - stripeOffset - stripeWidth)
          }
          new Graphics()
            .fillRect(stripeSize, stripePosition, Colors.PureBlack)
            .addTo(container)
        }
        container
      }
      _ = log.info("rendering grid texture")
      _ = app.renderer.render(gridContainer, gridTexture)
      gridBlob <- app.export(gridContainer)
      gridContent = gridBlob.encode
      _ = log.info("creating scanimation graphics")
      // scanimationTexture = RenderTexture.create(size.x, size.y)
      _ = log.info("rendering scanimation texture")
      //
      _ = log.info("done scanimating")
      result = CompleteScanimation(ImageContent("", null), ImageContent(gridContent, gridTexture))
    } yield result

    /** Clears the previous scanimation results */
    def clearScanimation(): Unit = {
      log.info("clearing the scanimation results")
      model.scanimation.reset
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
    * @param tick            the current update tick
    * @param frame           the current rendering frame
    * @param page            currently displayed scanimation page
    * @param frames          a list of frame images uploaded by the user
    * @param imageSize       the image resolution of the first loaded frame
    * @param frameCount      the number of scanimation frames after overlap
    * @param frameWidth      the width of the gap between grid lines in pixels
    * @param frameOverlap    the number of animation frames put into single scanimation frame
    * @param direction       the direction of scanimation grid movement
    * @param defaultSettings true if all of the settings are set to default values
    * @param canScanimate    true if the app is ready to run the scanimation
    * @param scanimation     the scanimation computing results
    */
  case class Model(tick: Writeable[Long] = Data(0),
                   frame: Writeable[Long] = Data(0),
                   page: Writeable[Page] = LazyData(router.parsePage),

                   frames: ListData[Frame] = ListData(),
                   imageSize: Writeable[Option[Vec2i]] = Data(None),
                   frameCount: Writeable[Option[Int]] = Data(None),
                   frameWidth: Writeable[Option[Int]] = Data(Some(Default.frameWidth)),
                   frameOverlap: Writeable[Option[Int]] = Data(Some(Default.frameOverlap)),
                   direction: Writeable[Directions.Value] = Data(Default.direction),
                   defaultSettings: Writeable[Boolean] = LazyData(true),
                   canScanimate: Writeable[Boolean] = LazyData(false),
                   scanimation: TransitionData[CompleteScanimation] = Data(Missing()))

  /** Defines the single animation frame
    *
    * @param name    the name of the imported file
    * @param size    the size of the imported frame
    * @param content the contents of the frame image
    */
  case class Frame(name: String, size: Vec2i, content: ImageContent)

  /** Contains builders for error codes */
  object ErrorCodes {
    def FrameReaderError: TransitionException = TransitionException("0000", "failed to read file")

    def FrameFormatError: TransitionException = TransitionException("0001", "failed to recognize file format")

    def FrameSizeError: TransitionException = TransitionException("0002", "failed to read frame size")

    def FramePixiError: TransitionException = TransitionException("0003", "failed to import frame as a texture")

    def FramesMissingError: TransitionException = TransitionException("0004", "at least one frame must be present")

    def FrameWidthError: TransitionException = TransitionException("0005", "invalid frame width")

    def FrameOverlapError: TransitionException = TransitionException("0006", "invalid frame overlap")

    def FrameIncompatibleSizeError(frame: Frame, expected: Vec2i): TransitionException = TransitionException("0007", s"frames must have the same size: expected size [${expected.x}x${expected.y}], found size [${frame.size.x}x${frame.size.y}] for frame [${frame.name}]")
  }

  /** Describes the URL encoded and GPU loaded image */
  case class ImageContent(url: String, texture: Texture)

  /** Contains scanimation processing results
    *
    * @param scanimation the image url of combined animation frames
    * @param grid        the image url of the scanimation grid with transparent background
    */
  case class CompleteScanimation(scanimation: ImageContent, grid: ImageContent)

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