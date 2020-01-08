package scanimation.pages

import scanimation.mvc._
import scanimation.pages.pages.PageLayout
import scanimation.util.logging.Logging

/** Dragons page layout */
object BuilderLayout extends PageLayout[BuilderPage] with Logging {
  override protected def logKey: String = "builder"

  /** Returns the parent box for the page layout when page opens */
  override def open(controller: Controller): Unit = {
  }

}