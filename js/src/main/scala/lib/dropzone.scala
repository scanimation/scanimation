package lib

import lib.facade.dropzone.Dropzone
import scanimation.box.Box
import scanimation.util.timer.Awaitor

import scala.scalajs.js.Dynamic.literal

/** Scala API for dropzone.
  * https://www.dropzonejs.com/
  */
object dropzone {

  implicit class DropzoneBoxOps[A <: Box](val box: A) extends AnyVal {
    /** Attaches a dropzone to a given box */
    def dropzone(url: String): A = {
      Awaitor.onceExists(s"[boxId='${box.id}']") { case (selector, _) =>
        val config = literal(url = url)
        new Dropzone(selector, config)
      }
      box
    }
  }

}