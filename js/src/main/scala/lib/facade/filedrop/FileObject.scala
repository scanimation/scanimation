package lib.facade.filedrop

import scala.scalajs.js

/** Data object passed to handler with each file uploaded */
@js.native
trait FileObject extends js.Object {
  /** This is the actual data of the file.
    * Depending on the options you set, this will be either a base64 encoded string, or the raw file contents. */
  val data: String = js.native

  /** The date that the file was last modified on. */
  val lastModified: js.Date = js.native

  /** The file name. */
  val name: String = js.native

  /** The size of the file, in bytes. */
  val size: Int = js.native

  /** The MIME type of the file. */
  val `type`: String = js.native
}