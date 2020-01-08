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

import scala.concurrent.Future

/** Starts the UI application */
//noinspection TypeAnnotation
object app extends App with GlobalContext with Logging {
  override protected def logKey: String = "app"

  config.setGlobalReader(JsReader)
  implicit val conf: ScanimationConfig = scanimation.conf.Config

  $ { () =>
    history.location.pathname match {
      case discord if discord.startsWith("/discord") =>
        queryParameter("code") match {
          case Some(code) => loginDiscord(code)
          case None =>
            log.warn("login error, redirecting to [/]")
            redirect("/")
        }
      case path =>
        startScanimation(path)
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
    future.whenFailed(up => log.error("failed to build ui", up))
  }

  /** Cleans up location after discord oauth2 flow */
  def loginDiscord(code: String)(implicit config: ScanimationConfig): Unit = for {
    user <- Future.successful(None) // post[LoginDiscord, User]("/api/discord", LoginDiscord(code))
    _ = log.info(s"logged in as [$user]")
    _ = redirectSilent("/")
    _ = startScanimation("/")
  } yield user

}