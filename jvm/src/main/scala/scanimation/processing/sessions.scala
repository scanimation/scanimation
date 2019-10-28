package scanimation.processing

import java.lang.System.currentTimeMillis
import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef}
import discord.DiscordUser

object sessions {

  /** Contains the session id */
  case class SessionId(id: String)

  /** Describes user session on the server */
  case class Session(id: SessionId, lastAccess: Long, discordUser: Option[DiscordUser])

  /** Wraps the reference to session manager actor */
  case class SessionManagerRef(ref: ActorRef)

  /** Manages user sessions */
  class SessionManager extends Actor with ActorLogging {
    private var sessions: Map[SessionId, Session] = Map.empty

    override def receive: Receive = {
      case EnsureSession(id) =>
        val session = id.flatMap(sessions.get).getOrElse(emptySession)
        sessions = sessions + (session.id -> session)
        sender ! session

      case UpdateSession(id, code) =>
        val session = sessions.getOrElse(id, emptySession)
        val updated = code.apply(session)
        sessions = sessions + (session.id -> updated)
        sender ! updated

      case ForgetSession(id) =>
        sessions = sessions - id
        self.tell(EnsureSession(None), sender)
    }

    private def emptySession: Session = Session(
      id = SessionId(UUID.randomUUID().toString),
      lastAccess = currentTimeMillis(),
      discordUser = None
    )
  }

  /** Requests to get session data for the requesting user */
  case class EnsureSession(id: Option[SessionId])

  /** Requests to update the session data */
  case class UpdateSession(id: SessionId, code: Session => Session)

  /** Removes the session with given id */
  case class ForgetSession(id: SessionId)

}