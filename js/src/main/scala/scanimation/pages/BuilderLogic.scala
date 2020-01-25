package scanimation.pages

import lib.filedrop._
import org.querki.jquery._
import org.scalajs.dom.raw.{FileReader, HTMLImageElement, HTMLInputElement}
import scanimation.common._
import scanimation.mvc._
import scanimation.ops._
import scanimation.pages.pages.PageLogic
import scanimation.util.global.GlobalContext
import scanimation.util.logging.Logging

import scala.concurrent.{Future, Promise}

/** Dragons page layout */
object BuilderLogic extends PageLogic[BuilderPage] with Logging with GlobalContext {
  override protected def logKey: String = "builder"

  private lazy val loading = $("#loading")

  /** Returns the parent box for the page layout when page opens */
  override def open(controller: Controller): Unit = {
    bindOverlays()
    bindFrames(controller)
    bindSettings(controller)
    loading.hidden()
  }

  private lazy val overlays = $(".overlay")
  private lazy val overlaysShadow = $(".overlay > div")
  private lazy val overlaysCancel = $(".overlay-close")

  /** Binds the overlay generic actions */
  def bindOverlays(): Unit = {
    overlays.hidden()
    overlaysShadow.click({ event: JQueryEventObject => event.stopPropagation() })
    overlaysCancel.click(() => overlays.hidden())
  }

  private lazy val framesDropzone = $("#frames-dropzone")
  private lazy val framesList = $("#frames-list")
  private lazy val framesAdd = $("#frames-add")
  private lazy val framesClear = $("#frames-clear")
  private lazy val framesClearOverlay = $("#frames-clear-overlay")
  private lazy val framesClearYes = $("#frames-clear-yes")
  private lazy val framesShow = $("#frames-show")
  private lazy val framesInput = $("#frames-input")

  /** Binds the frames section logic */
  def bindFrames(controller: Controller): Unit = {
    /** Describes a frame file */
    case class FrameFileAsync(name: String, tpe: String, content: Future[String])

    /** Processes the list of uploaded frame files */
    def readImages(controller: Controller, files: List[FrameFileAsync]): Unit = {
      for {
        _ <- UnitFuture
        filtered = files.filter(file => file.tpe.startsWith("image/")).sortBy(file => file.name)
        _ = log.info(s"processing [${filtered.size}/${files.size}] images")
        contents <- Future.sequence(filtered.map(file => file.content))
        _ = log.info(s"loaded [${contents.size}] images content")
        frames <- Future.sequence(filtered.zip(contents).map { case (file, content) =>
          val promise = Promise[Frame]
          val image = $("<img>").firstAs[HTMLImageElement]
          image.onload = { _ =>
            val width = if (image.naturalWidth > 0) image.naturalWidth else image.width
            val height = if (image.naturalHeight > 0) image.naturalHeight else image.height
            promise.success(Frame(
              name = file.name,
              size = width xy height,
              content = content
            ))
          }
          image.src = content
          promise.future
        })
        _ = controller.addFrames(frames)
      } yield ()
    }

    controller.model.frames.data /> {
      case Nil =>
        framesList.hidden()
        framesDropzone.visible()
        framesClear.disable()
        framesShow.disable()
      case _ =>
        framesDropzone.hidden()
        framesList.visible()
        framesClear.enable()
        framesShow.enable()
    }

    framesDropzone
      .click(() => framesInput.click())
      .filedrop(
        handler = { files =>
          val frames = files.map(file => FrameFileAsync(file.name, file.`type`, Future.successful(file.data)))
          readImages(controller, frames)
        },
        overClass = "dropping"
      )
    framesAdd.click(() => framesInput.click())
    framesInput.change(() => {
      val files = framesInput.firstAs[HTMLInputElement].files.asList
      val frames = files.map { file =>
        val promise = Promise[String]
        val reader = new FileReader()
        reader.onload = { _ => promise.success(reader.result.toString) }
        reader.readAsDataURL(file)
        FrameFileAsync(file.name, file.`type`, promise.future)
      }
      readImages(controller, frames)
    })
    framesClear.click(() => {
      framesClearOverlay.visible()
    })
    framesClearYes.click(() => {
      controller.clearFrames()
      framesClearOverlay.hidden()
    })
  }

  private lazy val settingsDirection = $("#settings-direction")
  private lazy val settingsFrameWidth = $("#settings-frame-width")
  private lazy val settingsFrameOverlap = $("#settings-frame-overlap")
  private lazy val settingsReset = $("#settings-reset")
  private lazy val settingsResetOverlay = $("#settings-reset-overlay")
  private lazy val settingsResetYes = $("#settings-reset-yes")

  /** Binds the settings section logic */
  def bindSettings(controller: Controller): Unit = {
    controller.model.direction.bindDropdown(
      settingsDirection,
      direction => direction.toString,
      string => Directions.values.find(direction => string.equalsIgnoreCase(direction.toString)).get,
      controller.setDirection
    )
    controller.model.frameWidth.bindValidatedText(
      settingsFrameWidth,
      value => value.toString,
      string => string.toInt,
      value => value > 0,
      controller.setFrameWidth
    )
    controller.model.frameOverlap.bindValidatedText(
      settingsFrameOverlap,
      value => value.toString,
      string => string.toInt,
      value => value > 0,
      controller.setFrameOverlap
    )
    (controller.model.direction && controller.model.frameWidth && controller.model.frameOverlap) /> { case ((direction, width), overlap) =>
      if (direction == Default.direction && width.contains(Default.frameWidth) && overlap.contains(Default.frameOverlap)) {
        settingsReset.disable()
      } else {
        settingsReset.enable()
      }
    }
    settingsReset.click(() => settingsResetOverlay.visible())
    settingsResetYes.click(() => {
      controller.resetSettings()
      settingsResetOverlay.hidden()
    })
  }

}