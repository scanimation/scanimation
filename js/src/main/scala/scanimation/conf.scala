package scanimation

import scanimation.config._
import scanimation.format._
import scanimation.util.http
import scanimation.util.logging.Logging

object conf extends Logging {
  override protected def logKey: String = "config"

  /** Reads the config keys from java runtime environment */
  object JsReader extends ConfigReader {
    private val params = http.queryParameters

    override def get(path: Path): Option[String] = {
      params.get(path.stringify).flatMap(v => v.headOption)
    }
  }

  /** General configuration for all projects
    *
    * @param server              the protocol and host part for server uris
    * @param client              the protocol and host part for client uris
    * @param discordLogin        the redirect url for discord login
    * @param logs                the logging levels configuration
    */
  case class ScanimationConfig(server: String,
                               client: String,
                               discordLogin: String,
                               logs: LogConfig)

  /** Configures logging on different levels */
  case class LogConfig(wire: Boolean,
                       debug: Boolean,
                       info: Boolean,
                       warnings: Boolean,
                       errors: Boolean)

  val DefaultConfig = ScanimationConfig(
    server = "http://127.0.0.1:8081",
    client = http.hostPortString,
    discordLogin = s"https://discordapp.com/api/oauth2/authorize?client_id=583316882002673683&redirect_uri=${http.hostPortString}/discord&response_type=code&scope=identify",
    logs = LogConfig(wire = false, debug = false, info = true, warnings = true, errors = true),
  )

  implicit val logConfigFormat: CF[LogConfig] = format5(LogConfig)
  implicit val scanimationConfigFormat: CF[ScanimationConfig] = format4(ScanimationConfig)

  lazy val Config: ScanimationConfig = configureNamespace("scanimation", Some(DefaultConfig))

}