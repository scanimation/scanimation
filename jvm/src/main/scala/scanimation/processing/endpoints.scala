package scanimation.processing

import akka.http.scaladsl.server.Directives._
import scanimation.conf.ScanimationConfig
import scanimation.processing.sessions.SessionManagerRef
import scanimation.protocol.LoginDiscord
import scanimation.util.akkautil._

//noinspection TypeAnnotation
object endpoints {
  val `GET /api/health` = get & path("api" / "health")

  def `POST /api/discord`(implicit manager: SessionManagerRef, config: ScanimationConfig) = post & path("api" / "discord") & session() & entity(as[LoginDiscord])

  def `GET /api/user`(implicit manager: SessionManagerRef, config: ScanimationConfig) = get & path("api" / "user") & session()

  def `POST /api/signout`(implicit manager: SessionManagerRef, config: ScanimationConfig) = post & path("api" / "signout") & session()
}