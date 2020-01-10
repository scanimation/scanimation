package lib

import lib.facade.dropzone.Dropzone
import org.querki.jquery.JQuery
import scanimation.common._

import scala.scalajs.js.Dynamic.literal

/** Scala API for dropzone.
  * https://www.dropzonejs.com/
  */
object dropzone {

  implicit class DropzoneJquery(val jquery: JQuery) extends AnyVal {
    def dropzone(): JQuery = {
      val id = jquery.attr("id").getOrElse {
        val generated = uuid
        jquery.attr("id", uuid)
        generated
      }
      new Dropzone(s"#$id", literal())
      jquery
    }
  }

  //  implicit class DropzoneBoxOps[A <: Box](val box: A) extends AnyVal {
  //    /** Attaches a dropzone to a given box */
  //    def dropzone(url: String): A = {
  //      Awaitor.onceExists(s"[boxId='${box.id}']") { case (selector, _) =>
  //        val config = literal(url = url)
  //        new Dropzone(selector, config)
  //      }
  //      box
  //    }
  //  }

}