package lib.facade.uri

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** Uri and query string parser.
  * http://medialize.github.io/URI.js */
@js.native
@JSGlobal("URI")
class URI extends js.Object {
  def this(uri: String) = this()

  /** Set data map. http://medialize.github.io/URI.js/docs.html#accessors-search */
  def search(data: js.Dictionary[js.Array[String]]): URI = js.native
}

object URI extends js.Object {
  /** Parses the passed query string into an object. Returns object {propertyName: propertyValue} */
  def parseQuery(query: String): js.Dictionary[Any] = js.native
}