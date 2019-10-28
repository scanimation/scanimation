package lib

import lib.facade.browser.Browser
import scanimation.util.logging.Logging

/** This is a package that attempts to detect a browser vendor and version
  * https://www.npmjs.com/package/detect-browser
  */
object browser extends Logging {
  override protected def logKey: String = "browser"

  /** Detected browser type */
  object Browsers extends Enumeration {
    val Firefox, Chrome, Other = Value
  }

  val tpe: Browsers.Value = Browser.name.toLowerCase match {
    case "chrome" => Browsers.Chrome
    case "firefox" => Browsers.Firefox
    case _ => Browsers.Other
  }

  log.info(s"detected browser [$tpe]")
}