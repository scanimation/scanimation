package scanimation.processing

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.stream.Materializer
import akka.util.Timeout
import com.typesafe.scalalogging.LazyLogging
import scanimation.binary._
import scanimation.common._
import scanimation.conf
import scanimation.conf.ScanimationConfig
import scanimation.processing.endpoints._
import scanimation.processing.sessions.{ForgetSession, Session, SessionManagerRef, UpdateSession}
import scanimation.protocol._
import scanimation.util.akkautil._

import scala.concurrent.ExecutionContext

object routes extends LazyLogging {

  /** Contains all of the projects configs */
  case class FullConfig(general: ScanimationConfig = conf.Config)

  /** Returns general routes */
  def generalRoutes()(implicit config: ScanimationConfig, system: ActorSystem, manager: SessionManagerRef, materializer: Materializer, ec: ExecutionContext): List[Route] = List(
    /** Returns 200 OK */
    `GET /api/health` {
      complete(HttpResponse(StatusCodes.OK))
    },

    /** Returns OptionUser(Some(user)) if browser session contains a valid login, or OptionUser(None) if user did not log in */
    `GET /api/user`.apply { session =>
      complete(session.discordUser.map(u => u.asUser))
    },

    /** Authorizes the user via discord using grant code */
    `POST /api/discord`.apply { (session, login) =>
      implicit val to: Timeout = Timeout.durationToTimeout(config.timeout)
      onSuccess(for {
        _ <- UnitFuture
        _ = logger.info("authorizing discord user")
        auth <- discord.authorize(login.code, config)
        _ = logger.info("reading discord user data")
        discordUser <- discord.selfUser(auth)
        _ = logger.info(s"successfully authorized as [$discordUser], updating session")
        updated <- (manager.ref ? UpdateSession(session.id, s => s.copy(discordUser = Some(discordUser)))).mapTo[Session]
        sessionId = updated.id
      } yield (discordUser, sessionId)) { (discordUser, sessionId) =>
        resetSession(sessionId) {
          complete(discordUser.asUser)
        }
      }
    },

    /** Signs out the user by refreshing the session */
    `POST /api/signout`.apply { session =>
      implicit val to: Timeout = Timeout.durationToTimeout(config.timeout)
      onSuccess(for {
        refreshed <- (manager.ref ? ForgetSession(session.id)).mapTo[Session]
      } yield refreshed.id) { sessionId =>
        resetSession(sessionId) {
          complete()
        }
      }
    }
  )
}