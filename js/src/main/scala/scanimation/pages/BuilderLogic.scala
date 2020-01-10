package scanimation.pages

import lib.filedrop._
import org.querki.jquery._
import scanimation.mvc._
import scanimation.pages.pages.PageLogic
import scanimation.util.logging.Logging

/** Dragons page layout */
object BuilderLogic extends PageLogic[BuilderPage] with Logging {
  override protected def logKey: String = "builder"

  private lazy val dropzone = $("#frames-dropzone")

  /** Returns the parent box for the page layout when page opens */
  override def open(controller: Controller): Unit = {
    dropzone.filedrop(
      handler = { files => files.foreach(file => log.info(file.name)) },
      overClass = "dropping"
    )
  }

}