package scanimation.util

import java.util.Base64

import akka.actor.Scheduler
import akka.http.scaladsl.marshalling.PredefinedToEntityMarshallers._
import akka.http.scaladsl.marshalling.PredefinedToResponseMarshallers._
import akka.http.scaladsl.marshalling.{ToEntityMarshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.headers.{HttpCookie, HttpCookiePair}
import akka.http.scaladsl.model.{HttpRequest, IllegalRequestException, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Directive1}
import akka.http.scaladsl.unmarshalling.PredefinedFromEntityUnmarshallers._
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, FromRequestUnmarshaller}
import akka.pattern.ask
import akka.stream.Materializer
import akka.util.Timeout
import scanimation.binary._
import scanimation.conf.ScanimationConfig
import scanimation.processing.sessions.{EnsureSession, Session, SessionId, SessionManagerRef}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object akkautil {
  /** Creates the entity parser from binary format */
  implicit def binaryFormatToEntityUnmarshaller[A](implicit format: BF[A]): FromEntityUnmarshaller[A] = {
    stringUnmarshaller.map { string =>
      val bytes = Base64.getDecoder.decode(string)
      val a = ByteList(bytes).toScala[A]()
      a
    }
  }

  /** Creates the request parser from binary format */
  implicit def binaryFormatToRequestUnmarshaller[A](implicit format: BF[A]): FromRequestUnmarshaller[A] = new FromRequestUnmarshaller[A] {
    override def apply(request: HttpRequest)(implicit ec: ExecutionContext, materializer: Materializer): Future[A] = {
      binaryFormatToEntityUnmarshaller[A](format).apply(request.entity)
    }
  }

  /** Creates the entity marshaller from binary format */
  implicit def binaryFormatToEntityMarshaller[A](implicit format: BF[A]): ToEntityMarshaller[A] = {
    stringMarshaller(`text/plain`).compose { value =>
      val bytes = value.toBinary.toByteArray
      val base64 = Base64.getEncoder.encodeToString(bytes)
      base64
    }
  }

  /** Creates the response marshaller from binary format */
  implicit def binaryFormatToResponseMarshaller[A](implicit format: BF[A]): ToResponseMarshaller[A] = {
    fromToEntityMarshaller()
  }

  /** Provides the session id for the requesting user */
  val sessionId: Directive1[Option[SessionId]] = optionalCookie("session").flatMap {
    case Some(HttpCookiePair(name, value)) =>
      provide(Some(SessionId(value)))
    case None =>
      provide(None)
  }

  /** Provides the session data for the requesting user */
  def session()(implicit manager: SessionManagerRef, config: ScanimationConfig): Directive1[Session] = sessionId.flatMap { id =>
    implicit val timeout: Timeout = Timeout.durationToTimeout(config.timeout)
    onSuccess((manager.ref ? EnsureSession(id)).mapTo[Session])
  }

  /** Provides the session data for the admin user */
  def adminSession()(implicit manager: SessionManagerRef, config: ScanimationConfig): Directive1[Session] = session().flatMap { session =>
    session.discordUser match {
      case Some(user) if user.isAdmin =>
        provide(session)
      case Some(_) =>
        throw IllegalRequestException(StatusCodes.Forbidden, "User is not an admin")
      case None =>
        throw IllegalRequestException(StatusCodes.Forbidden, "User is not logged in")
    }
  }

  /** Updates the session cookie */
  def resetSession(id: SessionId): Directive0 = setCookie(HttpCookie("session", value = id.id, httpOnly = true, secure = false))

  /** Attempts to execute future several times with a delay between attempts */
  def retryFuture[A](code: () => Future[A], attempts: Int, delay: FiniteDuration)(implicit ec: ExecutionContext, s: Scheduler): Future[A] = {
    code.apply().recoverWith {
      case NonFatal(up) =>
        val promise = Promise[A]()

        def rec(recAttempts: Int, lastFailure: Throwable): Unit = {
          if (recAttempts <= 0) {
            promise.failure(up)
          } else {
            delayFuture(code, delay).onComplete {
              case Success(value) => promise.success(value)
              case Failure(NonFatal(recUp)) => rec(attempts - 1, recUp)
            }
          }
        }

        rec(attempts, up)
        promise.future
    }
  }

  /** Attempts to execute code several times with a delay between attempts */
  def retryCode[A](code: () => A, attempts: Int, delay: FiniteDuration)(implicit ec: ExecutionContext, s: Scheduler): Future[A] = {
    retryFuture(() => Future(code.apply()), attempts, delay)
  }

  /** Executes the given future after the delay */
  def delayFuture[A](code: () => Future[A], delay: FiniteDuration)(implicit ec: ExecutionContext, s: Scheduler): Future[A] = {
    val promise = Promise[A]()
    s.scheduleOnce(delay, new Runnable {
      override def run(): Unit = code.apply().onComplete(result => promise.complete(result))
    })
    promise.future
  }

  /** Executes the given code after the delay */
  def delayCode[A](code: () => A, delay: FiniteDuration)(implicit ec: ExecutionContext, s: Scheduler): Future[A] = {
    delayFuture(() => Future(code.apply()), delay)
  }
}