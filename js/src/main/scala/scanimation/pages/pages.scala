package scanimation.pages

import org.querki.jquery._
import scanimation.common._
import scanimation.mvc.{Controller, Page}
import scanimation.router
import scanimation.router.Route
import scanimation.util.global.GlobalContext
import scanimation.util.http._
import scanimation.util.logging.Logging
import scanimation.util.timer.Awaitor

import scala.concurrent.Future

object pages extends GlobalContext with Logging {
  override protected def logKey: String = "pages"

  private val currentRoute: Writeable[Option[Route[Page]]] = Data(None)

  /** Binds the layouts to controller model */
  def start(controller: Controller): Future[Unit] = for {
    _ <- UnitFuture
    _ = log.info("loading styles")
    _ <- loadStyles()
    _ = log.info("styles loaded, binding router")
    _ = (controller.model.page.map(p => router.findRoute(p)) && currentRoute) /> {
      case (route, None) =>
        currentRoute.write(Some(route))
        route.logic.open(controller)
        route.logic.updateUntyped(controller.model.page())
      case (route, Some(current)) =>
        if (!current.eq(route)) {
          currentRoute.write(Some(route))
          current.logic.close(controller)
          route.logic.open(controller)
        }
        route.logic.updateUntyped(controller.model.page())
    }
    _ = log.info("started")
  } yield ()

  /** Loads LESS in local environment and CSS in production */
  def loadStyles(): Future[Unit] = {
    if (hostPortString.contains("127.0.0.1")) {
      $("head")
        .append($("""<link rel="stylesheet/less" type="text/css" href="/styles.less">"""))
        .append($("""<script src="https://cdnjs.cloudflare.com/ajax/libs/less.js/3.9.0/less.min.js"></script>"""))
    } else {
      $("head")
        .append($("""<link rel="stylesheet" type="text/css" href="/styles.css">"""))
    }
    val body = $("body")
    Awaitor.await(body.css("overflow") == "hidden")().future
  }

  /** Wraps the page layout */
  trait PageLogic[A <: Page] {
    /** Executed once the page is opened */
    def open(controller: Controller): Unit

    /** Is called after the page is opened when inner page values change */
    def update(page: A): Unit = {}

    /** Inner method to deal with page types */
    def updateUntyped(page: Page): Unit = update(page.asInstanceOf[A])

    /** Is called when another page is opened */
    def close(controller: Controller): Unit = {}
  }

}