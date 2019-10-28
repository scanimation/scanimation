package scanimation

import scanimation.binary._
import scanimation.common._
import scanimation.format._

/** Contains all messages to interact with server */
//noinspection TypeAnnotation
object protocol {

  /** Describes tileset stored in a file */
  case class TilesetAreas(areas: List[Rec2d])

  /** Logs in the discord user
    *
    * @param code the code value contained in discord redirection url as a query parameter
    */
  case class LoginDiscord(code: String)

  /** Contains discord user information
    *
    * @param id    the discord id of the user
    * @param name  the displayed discord name of the user
    * @param admin true, if user is recognized as project admin
    */
  case class User(id: String, name: String, admin: Boolean)

  implicit val Vec2dFormat = format2(Vec2d.apply)
  implicit val Rec2dFormat = format2(Rec2d.apply)
  implicit val TilesetAreasFormat = format1(TilesetAreas)
  implicit val LoginDiscordFormat = format1(LoginDiscord)
  implicit val UserFormat = format3(User)

}