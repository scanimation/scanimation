package lib.facade.filedrop

import org.querki.jquery.JQuery

import scala.scalajs.js

/** Imports FileDrop functions to JQuery
  * http://chrismbarr.github.io/FileDrop/
  */
@js.native
trait JQueryFiledrop extends JQuery {
  /** Turns jquery element into the file drop */
  def fileDrop(config: js.Dynamic = js.native): JQuery
}

object JQueryFiledrop {
  /** Allows to use JQuery with FileDrop extension */
  implicit def jquery2filedrop(jquery: JQuery): JQueryFiledrop = jquery.asInstanceOf[JQueryFiledrop]
}