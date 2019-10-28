package scanimation.pages

import scanimation.box.{Box, BoxContext}
import scanimation.common.{Data, Writeable}
import scanimation.mvc.{Controller, Page}
import scanimation.router.Route
import scanimation.{jqbox, router}

object pages {
  /** Binds the layouts to controller model */
  def start(controller: Controller): Unit = {
    val currentRoute: Writeable[Option[Route[Page]]] = Data(None)
    (controller.model.page.map(p => router.findRoute(p)) && currentRoute) /> {
      case (route, None) =>
        currentRoute.write(Some(route))
        val parent = route.layout.open(controller)
        jqbox.boxContext.root.sub(parent)
        route.layout.updateUntyped(controller.model.page())
      case (route, Some(current)) =>
        if (!current.eq(route)) {
          currentRoute.write(Some(route))
          current.layout.close(controller)
          val parent = route.layout.open(controller)
          jqbox.boxContext.root.sub(parent)
        }
        route.layout.updateUntyped(controller.model.page())
    }
  }

  /** Wraps the page layout */
  trait PageLayout[A <: Page] {
    /** Returns the parent box for the page layout when page opens */
    def open(controller: Controller): Box

    /** Is called after the page is opened when inner page values change */
    def update(page: A): Unit = {}

    /** Inner method to deal with page types */
    def updateUntyped(page: Page): Unit = update(page.asInstanceOf[A])

    /** Is called when another page is opened */
    def close(controller: Controller): Unit = {}
  }

  /** Page layout for only jq boxes */
  trait JqBoxLayout[A <: Page] extends PageLayout[A] {
    implicit val context: BoxContext = jqbox.boxContext
  }

}