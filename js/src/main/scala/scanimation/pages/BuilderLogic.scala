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

  private lazy val framesDropzone = $("#frames-dropzone")
  private lazy val framesAdd = $("#frames-add")
  private lazy val framesInput = $("#frames-input")

  /** Returns the parent box for the page layout when page opens */
  override def open(controller: Controller): Unit = {
    framesDropzone.filedrop(
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
  }

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

  /** Describes a frame file */
  case class FrameFileAsync(name: String, tpe: String, content: Future[String])

}