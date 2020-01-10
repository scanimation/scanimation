package lib

import lib.facade.filedrop.FileObject
import lib.facade.filedrop.JQueryFiledrop._
import org.querki.jquery.JQuery

import scala.scalajs.js
import scala.scalajs.js.Dynamic.literal

/**
  * @author WispY
  */
object filedrop {

  implicit class FiledropOps(val jquery: JQuery) extends AnyVal {
    /** Converts jquery element into file drop
      *
      * @param handler             the code that will accept the file list once it is dropped
      * @param overClass           when files are dragged over the browser window, this is the CSS class that will be applied to the selected element or the element specified with the addClassTo option
      * @param addClassTo          this is the element that will have the CSS class set with the overClass option applied to it; by default class will be applied to the original element
      * @param removeDataUriScheme when JavaScript reads the file data, it comes back as base64 encoded data which begins with something like 'data:text/plain;base64,';
      *                            when this option is enabled, this will be removed from the base64 string so that the pure base64 data can be processed or sent to a server;
      *                            it is recommended to keep this option enabled unless you need to work with dropped images in JavaScript
      * @param decodebase64        when JavaScript reads the file data, it comes back as base64 encoded data;
      *                            enabling this option will decode the base64 data into the raw file contents;
      *                            this is useful for pure JavaScript applications where you might want to parse file contents directly in the browser;
      *                            it is not recommended to use this option if you plan on sending the file contents to a server via AJAX
      * @return the same jquery element but with file drop applied
      */
    def filedrop(handler: List[FileObject] => Unit = { _ => },
                 overClass: String = "state-over",
                 addClassTo: Option[JQuery] = None,
                 removeDataUriScheme: Boolean = true,
                 decodebase64: Boolean = false): JQuery = {
      val handlerJs: js.Function1[js.Array[FileObject], Unit] = { files => handler.apply(files.toList) }
      jquery.fileDrop(literal(
        onFileRead = handlerJs,
        overClass = overClass,
        addClassTo = addClassTo.orNull,
        removeDataUriScheme = removeDataUriScheme,
        decodebase64 = decodebase64
      ))
    }
  }

}