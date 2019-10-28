package scanimation.processing

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Authorization, OAuth2BearerToken}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.Materializer
import com.typesafe.scalalogging.LazyLogging
import scanimation.common._
import scanimation.conf.ScanimationConfig
import scanimation.protocol.User
import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

import scala.concurrent.{ExecutionContext, Future}

object discord extends SprayJsonSupport with LazyLogging {

  /** https://discordapp.com/developers/docs/topics/oauth2 */
  case class DiscordTokenResponse(`access_token`: String,
                                  `token_type`: String,
                                  `expires_in`: Long,
                                  `refresh_token`: String,
                                  `scope`: String)

  /** Describes the logged in discord user */
  case class DiscordUser(id: String, name: String) {
    /** Converts the discord user to general user */
    def asUser(implicit config: ScanimationConfig): User = User(id, name, isAdmin)

    /** Returns true if user is one of the admins */
    def isAdmin(implicit config: ScanimationConfig): Boolean = config.discordAdmins.contains(id)
  }

  /** User in discord api */
  case class DiscordApiUser(id: String, username: String)

  implicit val discordTokenResponseFormat: RootJsonFormat[DiscordTokenResponse] = jsonFormat5(DiscordTokenResponse)
  implicit val discordUserFormat: RootJsonFormat[DiscordApiUser] = jsonFormat2(DiscordApiUser)

  /** Authorizes the user from given discord oauth2 code */
  def authorize(code: String, config: ScanimationConfig)(implicit s: ActorSystem, m: Materializer, ec: ExecutionContext): Future[Authorization] = for {
    _ <- UnitFuture
    request = FormData(
      "client_id" -> config.discordClient,
      "client_secret" -> config.discordSecret,
      "grant_type" -> "authorization_code",
      "code" -> code,
      "redirect_uri" -> config.discordRedirect,
      "scope" -> "identify"
    )
    entity <- Marshal(request).to[RequestEntity]
    response <- Http().singleRequest(HttpRequest(method = HttpMethods.POST, uri = "https://discordapp.com/api/v6/oauth2/token", entity = entity))
    _ <- if (response.status.isSuccess()) UnitFuture else Future.failed(IllegalRequestException(StatusCodes.Unauthorized, "Failed to authorize via Discord"))
    body <- Unmarshal(response).to[DiscordTokenResponse]
    header = Authorization(OAuth2BearerToken(body.`access_token`))
  } yield header

  /** Reads the discord user profile of requesting user */
  def selfUser(auth: Authorization)(implicit s: ActorSystem, m: Materializer, ec: ExecutionContext): Future[DiscordUser] = for {
    _ <- UnitFuture
    response <- Http().singleRequest(HttpRequest(method = HttpMethods.GET, uri = "https://discordapp.com/api/v6/users/@me").addHeader(auth))
    _ <- if (response.status.isSuccess()) UnitFuture else Future.failed(IllegalRequestException(StatusCodes.Unauthorized, "Failed to read Discord user"))
    body <- Unmarshal(response).to[DiscordApiUser]
    user = DiscordUser(body.id, body.username)
  } yield user

}