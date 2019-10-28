package scanimation.util

import lib.ffo
import scanimation.box.Font
import scanimation.common._
import scanimation.util.global.GlobalContext
import scanimation.util.logging.Logging

import scala.concurrent.Future

object fonts extends GlobalContext with Logging {
  override protected def logKey: String = "fonts"

  /** Loads given list of fonts */
  def load(fonts: List[Font]): Future[Unit] = {
    log.info(s"loading fonts [${fonts.mkString(",")}]")
    fonts.map(load).oneByOne.clear
  }

  /** Loads given font */
  def load(font: Font): Future[Unit] = (for {
    _ <- UnitFuture
    _ = log.info(s"loading font [$font]")
    _ <- ffo.load(font)
    _ = log.info(s"font successfully loaded [$font]")
  } yield ()).whenFailed(up => log.error(s"failed to load [$font]", up))
}