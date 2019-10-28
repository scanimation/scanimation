package scanimation

import scanimation.config._
import scanimation.format._

import scala.concurrent.duration._

object conf {

  /** Contains configuration general for all projects
    *
    * @param host            the host ip for server binding
    * @param port            the server http port
    * @param timeout         the general request timeout
    * @param discordClient   the id of discord application for oauth
    * @param discordSecret   the secret of discord application for oauth
    * @param discordRedirect the uri where discord redirected oauth request
    * @param discordAdmins   the list of user ids treated as discord admins
    */
  case class ScanimationConfig(host: String,
                               port: Int,
                               timeout: FiniteDuration,

                               discordClient: String,
                               discordSecret: String,
                               discordRedirect: String,
                               discordAdmins: List[String])

  val DefaultScanimationConfig = ScanimationConfig(
    host = "localhost",
    port = 8081,
    timeout = 30.seconds,
    discordClient = "changeme",
    discordSecret = "changeme",
    discordRedirect = "http://127.0.0.1:8080/discord",
    discordAdmins = "337379582770675712" :: "254380888152997889" :: Nil
  )

  implicit val reader: ConfigReader = JvmReader
  implicit val scanimationConfigFormat: CF[ScanimationConfig] = format7(ScanimationConfig)

  val Config: ScanimationConfig = configureNamespace("general", Some(DefaultScanimationConfig))

}