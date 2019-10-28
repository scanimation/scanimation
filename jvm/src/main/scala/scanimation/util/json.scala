package scanimation.util

import scanimation.common._
import spray.json._

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object json {
  implicit val finiteDurationJsonFormat: RootJsonFormat[FiniteDuration] = new RootJsonFormat[FiniteDuration] {
    override def read(json: JsValue): FiniteDuration = json match {
      case JsString(string) => Try(string.duration) match {
        case Success(value) => value
        case Failure(NonFatal(up)) => throw DeserializationException(s"cannot parse string as duration: $string", up)
      }
      case other => throw DeserializationException(s"cannot parse non-string as duration: $other")
    }

    override def write(obj: FiniteDuration): JsValue = JsString(obj.prettyString)
  }
}