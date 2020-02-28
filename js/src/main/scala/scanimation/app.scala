package scanimation

import lib.history
import org.querki.jquery._
import scanimation.common._
import scanimation.conf.{JsReader, ScanimationConfig}
import scanimation.mvc._
import scanimation.util._
import scanimation.util.global.GlobalContext
import scanimation.util.http._
import scanimation.util.logging.Logging

/** Starts the UI application */
object app extends App with GlobalContext with Logging {
  override protected def logKey: String = "app"

  config.setGlobalReader(JsReader)
  implicit val conf: ScanimationConfig = scanimation.conf.Config

  $ { () =>
    if (isHttps || isLocalhost) {
      startScanimation(history.location.pathname)
    } else {
      redirectFull(httpsString)
    }
  }

  /** Launches the application */
  def startScanimation(path: String): Unit = {
    val model = Model()
    val controller = Controller(model, conf)
    val future = for {
      _ <- UnitFuture
      _ <- spring.load()(controller)
      _ <- animation.load()(controller)
      _ <- controller.start(path)
    } yield ()
    future.whenFailed { case up => log.error("failed to build ui", up) }
  }

}