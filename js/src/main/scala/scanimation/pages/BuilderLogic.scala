package scanimation.pages

import lib.facade.pixi.{Application, GlobalLoader, Sprite}
import lib.filedrop._
import lib.pixi._
import org.querki.jquery._
import org.scalajs.dom.raw.{Event, FileReader, HTMLImageElement, HTMLInputElement, URL}
import scanimation.common.Transition.{Failed, Loaded, Loading, Missing}
import scanimation.common._
import scanimation.mvc._
import scanimation.ops._
import scanimation.pages.pages.PageLogic
import scanimation.util.global.GlobalContext
import scanimation.util.logging.Logging
import scanimation.util.timer.Timer

import scala.concurrent.{Future, Promise}
import scala.scalajs.js.Dynamic

/** Dragons page layout */
object BuilderLogic extends PageLogic[BuilderPage] with Logging with GlobalContext {
  override protected def logKey: String = "builder"

  private lazy val loading = $("#loading")

  /** Returns the parent box for the page layout when page opens */
  override def open(controller: Controller): Unit = {
    bindOverlays()
    bindPreview(controller)
    bindFrames(controller)
    bindSettings(controller)
    bindScanimate(controller)
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
  private lazy val frameItemTemplate = framesList.find(".row").detach()
  private lazy val framesAdd = $("#frames-add")
  private lazy val framesClear = $("#frames-clear")
  private lazy val framesClearOverlay = $("#frames-clear-overlay")
  private lazy val framesClearYes = $("#frames-clear-yes")
  private lazy val framesShow = $("#frames-show")
  private lazy val framesInput = $("#frames-input")
  private lazy val framesErrorsOverlay = $("#frames-errors-overlay")
  private lazy val framesErrorsName = $("#frames-errors-name")
  private lazy val framesErrorsCode = $("#frames-errors-code")
  private lazy val framesErrorsDescription = $("#frames-errors-description")
  private lazy val framesErrorsYes = $("#frames-errors-yes")

  /** Binds the frames section logic */
  def bindFrames(controller: Controller): Unit = {
    /** Loads the frame content and calculates the size */
    def readFrame(name: String, tpe: String, contentFuture: Future[String]): Frame = {
      val transition: TransitionData[FrameData] = Data(Missing())
      val future = for {
        _ <- UnitFuture
        _ = log.info(s"checking frame [$name] mime type [$tpe]")
        _ <- if (tpe.startsWith("image/")) UnitFuture else Future.failed(ErrorCodes.FrameFormatError)
        _ = log.info(s"loading frame [$name] content")
        content <- contentFuture
        _ = log.info(s"calculating frame [$name] size")
        image <- readImage(content)
        size = {
          val width = if (image.naturalWidth > 0) image.naturalWidth else image.width
          val height = if (image.naturalHeight > 0) image.naturalHeight else image.height
          width xy height
        }
        _ = log.info(s"loading frame [$name] into pixi")
        texture <- GlobalLoader.loadAsync(name, content).mapFailure { case _ => ErrorCodes.FramePixiError }
        _ = log.info(s"frame [$name] fully loaded")
      } yield FrameData(size, content, texture)
      future.transition(transition)
      Frame(name, transition)
    }

    /** Converts frame content into an image */
    def readImage(content: String): Future[HTMLImageElement] = {
      val promise = Promise[HTMLImageElement]
      val image = $("<img>").firstAs[HTMLImageElement]
      image.onload = { _ => promise.success(image) }
      image.addEventListener("error", (_: Event) => promise.failure(ErrorCodes.FrameSizeError))
      image.src = content
      promise.future
    }

    /** Updates the indexes withing the frame list */
    def reindexFrameList(): Unit = {
      controller.model.frames.ids.zipWithIndex.foreach { case (otherId, index) =>
        otherId.item.find(".index").text(s"${index + 1}.")
      }
    }

    implicit val listenerId: ListenerId = ListenerId()
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
    controller.model.frames.onAdd { case (id, frame) =>
      val item = frameItemTemplate.clone()
      item.id(id)
      item.find(".name").text(frame.name)
      item.find(".remove").click(() => controller.removeFrame(id))
      item.find(".up").click(() => controller.moveFrameUp(id))
      item.find(".down").click(() => controller.moveFrameDown(id))
      item.click(() => {
        frame.data() match {
          case Failed(start, end, code, reason) =>
            framesErrorsName.text(s"Name: ${frame.name}")
            framesErrorsCode.text(s"Code: $code")
            framesErrorsDescription.text(s"Reason: $reason")
            framesErrorsYes.unbind().click(() => {
              controller.removeFrame(id)
              framesErrorsOverlay.hidden()
            })
            framesErrorsOverlay.visible()
          case _ => controller.selectFrame(id)
        }
      })
      framesList.append(item)
      reindexFrameList()
      frame.data /> { case value =>
        item.removeClass("loading")
        item.removeClass("error")
        item.removeClass("ready")
        value match {
          case Loading(start) => item.addClass("loading")
          case Failed(start, end, code, reason) => item.addClass("error")
          case Loaded(start, end, v) => item.addClass("ready")
          case _ =>
        }
      }
    }
    controller.model.frames.onRemove { case (id, frame) =>
      id.item.detach()
      reindexFrameList()
      frame.data.forget()
    }
    controller.model.frames.onOrder { case ids =>
      ids.foreach { id => id.item.detach().appendTo(framesList) }
      reindexFrameList()
    }
    controller.model.frames.onSelect { case ids =>
      controller.model.frames.ids.foreach { id => id.item.removeClass("selected") }
      ids.foreach { id => id.item.addClass("selected") }
    }

    framesDropzone
      .click(() => framesInput.click())
      .filedrop(
        handler = { files =>
          val frames = files.map(file => readFrame(file.name, file.`type`, Future.successful(file.data)))
          controller.addFrames(frames)
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
        reader.onerror = { _ => promise.failure(ErrorCodes.FrameReaderError) }
        reader.readAsDataURL(file)
        readFrame(file.name, file.`type`, promise.future)
      }
      controller.addFrames(frames)
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
  private lazy val settingsImageSize = $("#settings-image-size")
  private lazy val settingsFrameCount = $("#settings-frame-count")
  private lazy val settingsFrameWidth = $("#settings-frame-width")
  private lazy val settingsFrameOverlap = $("#settings-frame-overlap")
  private lazy val settingsReset = $("#settings-reset")
  private lazy val settingsResetOverlay = $("#settings-reset-overlay")
  private lazy val settingsResetYes = $("#settings-reset-yes")

  /** Binds the settings section logic */
  def bindSettings(controller: Controller): Unit = {
    controller.model.imageSize /> {
      case Some(size) => settingsImageSize.text(s"${size.x} x ${size.y}")
      case None => settingsImageSize.text("N/A")
    }
    controller.model.frameCount /> {
      case Some(count) => settingsFrameCount.text(count.toString)
      case None => settingsFrameCount.text("N/A")
    }
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
    controller.model.defaultSettings /> {
      case true => settingsReset.disable()
      case false => settingsReset.enable()
    }
    settingsReset.click(() => settingsResetOverlay.visible())
    settingsResetYes.click(() => {
      controller.resetSettings()
      settingsResetOverlay.hidden()
    })
  }

  private lazy val scanimate = $("#scanimate")
  private lazy val scanimating = $("#scanimating")
  private lazy val scanimationErrorsOverlay = $("#scanimation-errors-overlay")
  private lazy val scanimationErrorsCode = $("#scanimation-errors-code")
  private lazy val scanimationErrorsDescription = $("#scanimation-errors-description")
  private lazy val resultsSection = $("#results-section")
  private lazy val scanimationReset = $("#scanimation-reset")
  private lazy val scanimationImage = $("#scanimation-image")

  /** Binds the scanimate section logic */
  def bindScanimate(controller: Controller): Unit = {
    controller.model.canScanimate /> { case value => scanimate.enable(value) }
    (controller.model.scanimation && controller.model.canScanimate) /> {
      case (Missing(), false) =>
        scanimate.visible().disable()
        scanimating.hidden()
        resultsSection.hidden()
      case (Missing(), true) =>
        scanimate.visible().enable()
        scanimating.hidden()
        resultsSection.hidden()
      case (Loading(start), _) =>
        scanimate.hidden()
        scanimating.visible()
        resultsSection.hidden()
      case (Failed(start, end, code, reason), _) =>
        scanimationErrorsCode.text(s"Code: $code")
        scanimationErrorsDescription.text(s"Reason: $reason")
        scanimationErrorsOverlay.visible()
        controller.clearScanimation()
      case (Loaded(start, end, result), _) =>
        scanimate.hidden()
        scanimating.hidden()
        resultsSection.visible()
    }
    scanimate.click(() => controller.scanimate())
    scanimationReset.click(() => controller.clearScanimation())
  }

  private lazy val previewWrapper = $("#preview-wrapper")

  /** Binds the preview section logic */
  def bindPreview(controller: Controller): Unit = {
    val preview = new Application(Dynamic.literal(
      width = 1,
      height = 1,
      antialias = true,
      transparent = false,
      resolution = 1
    ))
    val previewCanvas = $(preview.renderer.view)
    previewCanvas.attr("id", "preview")
    preview.renderer.backgroundColor = Colors.Grey200.toDouble
    preview.renderer.autoResize = true
    previewWrapper.append($(preview.view))

    val previewSize: Writeable[Vec2d] = LazyData(0.0 xy 0.0)
    Timer.schedule(20, () => previewSize.write(previewWrapper.width() xy previewWrapper.height()))
    previewSize /> { case size => preview.renderer.resize(size.x, size.y) }

    val selectedFrame: Writeable[Option[FrameData]] = LazyData(None)
    controller.model.frames.onSelect { case ids =>
      selectedFrame.write(
        ids
          .headOption
          .flatMap(id => controller.model.frames.read(id))
          .flatMap { frame =>
            frame.data.read match {
              case Loaded(start, end, data@FrameData(size, content, texture)) => Some(data)
              case _ => None
            }
          }
      )
    }

    val root = preview.stage.sub
    val frameSprite = new Sprite().anchorAtCenter.addTo(root)
    val scanimationContainer = root.sub
    val scanimationSprite = new Sprite().anchorAtCenter.addTo(scanimationContainer).scaleTo(0.5)
    selectedFrame /> {
      case Some(FrameData(size, content, texture)) =>
        frameSprite.texture = texture
        frameSprite.visible = true
        scanimationSprite.texture = texture
      case None =>
        frameSprite.visible = false
    }

    (previewSize && selectedFrame) /> {
      case (size, Some(frame)) =>
        val scale = (size / frame.size).min min 1
        frameSprite.scaleTo(scale)
        frameSprite.positionAt(size / 2)
    }

    scanimationImage.click(() => {
      preview.renderer.extract.canvas(scanimationContainer).toBlob(blob => blob.download("scanimation.png"), "image/png")
    })
  }

}