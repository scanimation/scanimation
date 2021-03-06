package scanimation.pages

import lib.facade.pixi._
import lib.filedrop._
import lib.pixi._
import org.querki.jquery._
import org.scalajs.dom.raw.{Event, FileReader, HTMLImageElement, HTMLInputElement}
import scanimation.common.Transition._
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

  private val frameDuration = 5
  private val gridOverlap = 0.75

  private val playingAnimation: Writeable[Boolean] = LazyData(false)
  private val animationFrameId: Writeable[Option[ListElementId]] = LazyData(None)
  private val playingScanimation: Writeable[Boolean] = LazyData(false)
  private val scanimationStartFrame: Writeable[Long] = LazyData(0L)
  private val selectedFrame: Writeable[Option[Frame]] = LazyData(None)

  private lazy val loading = $("#loading")

  /** Returns the parent box for the page layout when page opens */
  override def open(controller: Controller): Unit = {
    bindOverlays()
    bindInternal(controller)
    val loadContainer = bindPreview(controller)
    bindFrames(controller, loadContainer)
    bindSettings(controller)
    bindScanimate(controller)
    bindHelp()
    loading.hidden()
  }

  /** Binds the internal states of the UI */
  def bindInternal(controller: Controller): Unit = {
    controller.model.frames.onSelect { case ids =>
      selectedFrame.write(
        ids.headOption.flatMap(id => controller.model.frames.read(id))
      )
    }
    selectedFrame /> {
      case Some(frame) =>
        playingAnimation.write(false)
        playingScanimation.write(false)
    }
    controller.model.frames.data /> {
      case Nil =>
        playingAnimation.write(false)
        playingScanimation.write(false)
    }
    playingAnimation /> {
      case true =>
        controller.deselectFrames()
        playingScanimation.write(false)
    }
    playingScanimation /> {
      case true =>
        controller.deselectFrames()
        playingAnimation.write(false)
    }
    controller.model.scanimation /> {
      case Missing() =>
        playingScanimation.write(false)
    }
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
  private lazy val framesListItemTemplate = framesList.find(".row").detach()
  private lazy val framesAdd = $("#frames-add")
  private lazy val framesClear = $("#frames-clear")
  private lazy val framesClearOverlay = $("#frames-clear-overlay")
  private lazy val framesClearYes = $("#frames-clear-yes")
  private lazy val framesShow = $("#frames-show")
  private lazy val framesInput = $("#frames-input")
  private lazy val framesLoadingOverlay = $("#frames-loading-overlay")
  private lazy val framesLoadingOverlayClose = framesLoadingOverlay.find(".overlay-close")
  private lazy val framesLoadingList = $("#frames-loading-list")
  private lazy val framesLoadingListItemTemplate = framesLoadingList.find(".row").detach()

  /** Describes the frame before it's loaded */
  case class AsyncFrame(name: String, tpe: String, contentFutureCode: () => Future[String])

  /** Binds the frames section logic */
  def bindFrames(controller: Controller, pixiContainer: Container): Unit = {
    /** Loads a list of frames and shows overlay */
    def readFrames(list: List[AsyncFrame]): Unit = for {
      _ <- UnitFuture
      _ = framesLoadingListItemTemplate // forced lazy loading
      _ = framesLoadingOverlay.visible()
      _ = framesLoadingOverlayClose.disable()
      _ = framesLoadingList.children().detach()
      _ = pixiContainer.removeChildren.visibleTo(true)
      sorted = list.sortBy(frame => frame.name)
      frames <- Future.sequence(sorted.map(async => readFrame(async)))
      _ = framesLoadingOverlayClose
        .click(() => {
          controller.addFrames(frames.flatten)
          framesLoadingOverlayClose.unbind()
          framesLoadingOverlay.hidden()
        })
        .enable()
      _ = pixiContainer.removeChildren.visibleTo(false)
    } yield ()

    /** Loads the frame content and calculates the size */
    def readFrame(async: AsyncFrame): Future[Option[Frame]] = {
      val name = async.name
      val tpe = async.tpe
      val item = framesLoadingListItemTemplate.clone()
      val id = uuid
      item.id(id)
      item.addClass("loading")
      item.find(".name").text(name)
      item.appendTo(framesLoadingList)
      log.info(s"appended frame loading item [$id], current size [${framesLoadingList.children().length}]")
      val future = for {
        _ <- UnitFuture
        _ = log.info(s"checking frame [$name] mime type [$tpe]")
        _ <- if (tpe.startsWith("image/")) UnitFuture else Future.failed(ErrorCodes.FrameFormatError)
        _ = log.info(s"loading frame [$name] content")
        content <- async.contentFutureCode.apply()
        _ = log.info(s"calculating frame [$name] size")
        image <- readImage(content)
        size = {
          val width = if (image.naturalWidth > 0) image.naturalWidth else image.width
          val height = if (image.naturalHeight > 0) image.naturalHeight else image.height
          width xy height
        }
        _ = log.info(s"loading frame [$name] into pixi")
        texture <- SharedLoader.loadAsync(name, content).mapFailure { case _ => ErrorCodes.FramePixiError }
        _ = log.info(s"creating frame [$name] sprite in pixi")
        _ = new Sprite(texture).addTo(pixiContainer)
        _ = log.info(s"frame [$name] fully loaded")
        _ = item.removeClass("loading").addClass("success")
      } yield Some(Frame(name, size, ImageContent(content, texture)))
      future.recover {
        case up: TransitionException =>
          item.removeClass("loading").addClass("failure")
          item.find(".description").text(s"code-${up.code}: ${up.reason}")
          None
      }
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
      val item = framesListItemTemplate.clone()
      item.id(id)
      item.find(".name").text(frame.name)
      item.find(".remove").click(() => controller.removeFrame(id))
      item.find(".up").click(() => controller.moveFrameUp(id))
      item.find(".down").click(() => controller.moveFrameDown(id))
      item.click(() => controller.selectFrame(id))
      framesList.append(item)
      reindexFrameList()
    }
    controller.model.frames.onRemove { case (id, frame) =>
      id.item.detach()
      reindexFrameList()
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
          val frames = files.map { file => AsyncFrame(file.name, file.`type`, () => Future.successful(file.data)) }
          readFrames(frames)
        },
        overClass = "dropping"
      )
    framesAdd.click(() => framesInput.click())
    framesInput.change(() => {
      val files = framesInput.firstAs[HTMLInputElement].files.asList
      val frames = files.map { file =>
        AsyncFrame(file.name, file.`type`, () => {
          val promise = Promise[String]
          val reader = new FileReader()
          reader.onload = { _ => promise.success(reader.result.toString) }
          reader.onerror = { _ => promise.failure(ErrorCodes.FrameReaderError) }
          reader.readAsDataURL(file)
          promise.future
        })
      }
      readFrames(frames)
    })
    framesClear.click(() => {
      framesClearOverlay.visible()
    })
    framesClearYes.click(() => {
      controller.clearFrames()
      framesClearOverlay.hidden()
    })
    framesShow.click(() => {
      if (playingAnimation()) {
        playingAnimation.write(false)
        animationFrameId().foreach(id => controller.selectFrame(id))
      } else {
        playingAnimation.write(true)
      }
    })
    playingAnimation /> {
      case false =>
        framesShow.removeClass("playing").addClass("stopped")
      case true =>
        framesShow.addClass("playing").removeClass("stopped")
    }
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
  private lazy val scanimationGrid = $("#scanimation-grid")
  private lazy val scanimationPlay = $("#scanimation-play")

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
    scanimationPlay.click(() => {
      if (playingScanimation()) {
        playingScanimation.write(false)
        controller.model.frames.ids.headOption.foreach(id => controller.selectFrame(id))
      } else {
        scanimationStartFrame.write(controller.model.frame())
        playingScanimation.write(true)
      }
    })
    playingScanimation /> {
      case false =>
        scanimationPlay.removeClass("playing").addClass("stopped")
      case true =>
        scanimationPlay.addClass("playing").removeClass("stopped")
    }
  }

  private lazy val previewWrapper = $("#preview-wrapper")

  /** Binds the preview section logic */
  def bindPreview(controller: Controller): Container = {
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

    val root = preview.stage.sub
    val loadContainer = root.sub.scaleTo(0.001)
    val selectedSprite = new Sprite().anchorAtCenter.addTo(root)
    (selectedFrame && previewSize) /> {
      case (Some(Frame(name, frameSize, ImageContent(url, texture))), size) =>
        val scale = (size / frameSize).min min 1
        selectedSprite
          .scaleTo(scale)
          .positionAt(size / 2)
          .textureTo(texture)
          .visibleTo(true)
      case _ =>
        selectedSprite
          .clearTexture
          .visibleTo(false)
    }

    val animationSprite = new Sprite().anchorAtCenter.addTo(root)
    (playingAnimation && controller.model.frame && previewSize) /> {
      case ((true, frameId), size) =>
        val frames = controller.model.frames.data()
        val index = ((frameId / frameDuration) % frames.size).toInt
        val frame = frames(index)
        val scale = (size / frame.size).min min 1
        animationFrameId.write(controller.model.frames.ids.lift(index))
        animationSprite
          .scaleTo(scale)
          .positionAt(size / 2)
          .textureTo(frame.content.texture)
          .visibleTo(true)
      case _ =>
        animationSprite.visibleTo(false)
    }

    val scanimationContainer = root.sub.visibleTo(false)
    val scanimationSprite = new Sprite().anchorAtCenter.addTo(scanimationContainer)
    val gridSprite = new Sprite().anchorAtCenter.withBlendMode(BlendModes.MULTIPLY).addTo(scanimationContainer)
    controller.model.scanimation /> {
      case Loaded(start, end, scanimation) =>
        scanimationSprite.textureTo(scanimation.scanimation.texture)
        gridSprite.textureTo(scanimation.grid.texture)
    }
    (playingScanimation && controller.model.frame && scanimationStartFrame && previewSize) /> {
      case (((true, frameId), startFrame), size) =>
        scanimationContainer.visibleTo(true)
        for {
          frameSize <- controller.model.imageSize()
          frameWidth <- controller.model.frameWidth()
          direction = controller.model.direction()
          frameCount <- controller.model.frameCount()
        } yield {
          val scale = (size / frameSize).min min 1
          scanimationSprite
            .scaleTo(scale)
            .positionAt(size / 2)

          val areaLength = direction match {
            case Directions.Left | Directions.Right => frameSize.x
            case Directions.Up | Directions.Down => frameSize.y
          }
          val repeats = areaLength / (frameWidth * frameCount) * (gridOverlap * 2)
          val totalDuration = frameDuration * frameCount * repeats
          val progress = ((frameId - startFrame) % totalDuration.toLong) / totalDuration
          val start = direction match {
            case Directions.Left => (areaLength * gridOverlap) xy 0
            case Directions.Right => (areaLength * -gridOverlap) xy 0
            case Directions.Up => 0 xy (areaLength * gridOverlap)
            case Directions.Down => 0 xy (areaLength * -gridOverlap)
          }
          val end = Vec2d.Zero - start
          val position = start.progress(progress, end) * scale + size / 2
          gridSprite
            .scaleTo(scale)
            .positionAt(position)
        }
      case _ =>
        scanimationContainer.visibleTo(false)
    }

    scanimationImage.click(() => {
      controller.model.scanimation().valueOpt.foreach(value => value.scanimation.url.download("scanimation.png"))
    })
    scanimationGrid.click(() => {
      controller.model.scanimation().valueOpt.foreach(value => value.grid.url.download("grid.png"))
    })

    loadContainer
  }

  private lazy val helpFramesOverlay = $("#help-frames-overlay")
  private lazy val helpOverlapOverlay = $("#help-overlap-overlay")
  private lazy val helpImageSizeOverlay = $("#help-image-size-overlay")
  private lazy val helpFrameCountOverlay = $("#help-frame-count-overlay")
  private lazy val helpDirectionOverlay = $("#help-direction-overlay")
  private lazy val helpFrameWidthOverlay = $("#help-frame-width-overlay")
  private lazy val helpFramesButton = $("#frames-help")
  private lazy val helpOverlapButton = $("#settings-frame-overlap-help")
  private lazy val helpImageSizeButton = $("#settings-image-size-help")
  private lazy val helpFrameCountButton = $("#settings-frame-count-help")
  private lazy val helpDirectionButton = $("#settings-direction-help")
  private lazy val helpFrameWidthButton = $("#settings-frame-width-help")

  /** Binds help buttons to their respective overlays */
  def bindHelp(): Unit = {
    helpOverlapButton.click(() => helpOverlapOverlay.visible())
    helpFramesButton.click(() => helpFramesOverlay.visible())
    helpImageSizeButton.click(() => helpImageSizeOverlay.visible())
    helpFrameCountButton.click(() => helpFrameCountOverlay.visible())
    helpDirectionButton.click(() => helpDirectionOverlay.visible())
    helpFrameWidthButton.click(() => helpFrameWidthOverlay.visible())
  }

}