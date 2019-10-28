package lib

import lib.facade.uri.URI

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

/** Scala api for URI library.
  * http://medialize.github.io/URI.js */
object uri {
  /** Parses the passed query into multimap */
  def parseQuery(query: String): Map[String, List[String]] = {
    URI.parseQuery(query).toMap.mapValues {
      case list: js.Array[String] => list.toList
      case null => Nil
      case single => List(single.toString)
    }
  }

  /** Creates a uri from given location string */
  def uri(base: String): URI = new URI(base)

  implicit class UriOps(val uri: URI) extends AnyVal {
    /** Appends the given parameters to query string of the uri */
    def appendQuery(query: Map[String, List[String]]): URI = {
      val props = query.map { case (key, values) => key -> values.toJSArray }.toList
      val dictionary = js.Dictionary.apply(props: _*)
      uri.search(dictionary)
    }
  }

}