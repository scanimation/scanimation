package scanimation.util

import java.net.URI
import java.util.Base64

import lib.{history, uri}
import scanimation.binary._
import scanimation.conf.ScanimationConfig
import scanimation.util.logging.Logging
import org.scalajs.dom.{XMLHttpRequest, document, window}

import scala.concurrent.{Future, Promise}
import scala.scalajs.js.URIUtils
import scala.util.Try

object http extends Logging {
  override protected def logKey: String = "http"

  /** Performs the get http request */
  def get[A](path: String, parameters: List[(String, Any)] = Nil)(implicit config: ScanimationConfig, aformat: BF[A]): Future[A] = {
    request("GET", path, parameters, None, response = true, api = true)(config, unitFormat, aformat)
  }

  /** Retrieves the static resource for UI */
  def resource[A](path: String, parameters: List[(String, Any)] = Nil)(implicit aformat: BF[A]): Future[A] = {
    request("GET", path, parameters, None, response = true, api = false)(null, unitFormat, aformat)
  }

  /** Performs the post http request */
  def post[A, B](path: String, body: A)(implicit config: ScanimationConfig, aformat: BF[A], bformat: BF[B]): Future[B] = {
    request("POST", path, Nil, Some(body), response = true, api = true)(config, aformat, bformat)
  }

  /** Performs the post http request without parsing response */
  def postUnit[A](path: String, body: A)(implicit config: ScanimationConfig, aformat: BF[A]): Future[Unit] = {
    request("POST", path, Nil, Some(body), response = false, api = true)(config, aformat, unitFormat)
  }

  /** Redirects to given path within same server */
  def redirect(path: String)(implicit config: ScanimationConfig): Unit = {
    window.location.href = s"${config.client}$path"
  }

  /** Redirects to given uri */
  def redirectFull(uri: String): Unit = {
    window.location.href = uri
  }

  /** Opens the given uri in a new tab */
  def newTab(uri: String): Unit = {
    window.open(uri, "_blank")
  }

  /** Redirects to given url without reloading the page */
  def redirectSilent(path: String): Unit = {
    history.push(path)
  }

  /** Updates the page title */
  def updateTitle(title: String): Unit = {
    document.title = title
  }

  /** Preforms the http request to given uri */
  def request[A, B](method: String, path: String, parameters: List[(String, Any)], body: Option[A], response: Boolean, api: Boolean)(implicit config: ScanimationConfig, aformat: BF[A], bformat: BF[B]): Future[B] = {
    val query = if (parameters.isEmpty) "" else {
      val string = parameters
        .map { case (key, value) => s"${encode(key)}=${encode(value)}" }
        .mkString("&")
      s"?$string"
    }

    val requestBase64 = body.map { a =>
      val bytes = a.toBinary.toByteArray
      Base64.getEncoder.encodeToString(bytes)
    }

    val promise = Promise[B]()

    val request = new XMLHttpRequest()
    request.withCredentials = true
    request.onload = { _ =>
      if (response) {
        promise.complete(Try {
          log.info(s"parsing response from [$path]")
          val base64 = request.response.asInstanceOf[String]
          val bytes = Base64.getDecoder.decode(base64)
          val b = ByteList(bytes).toScala[B]()
          log.info(s"response successfully parsed from [$path]")
          b
        })
      } else {
        promise.success(().asInstanceOf[B])
      }
    }
    request.onerror = { _ =>
      val message = s"failed http request [$method $path] with status [${request.status} - ${request.statusText}]"
      val up = new RuntimeException(message)
      log.error(message, up)
      promise.failure(up)
    }
    val uri = if (api) s"${config.server}$path$query" else s"$path$query"
    request.open(method, uri, async = true)
    request.responseType = "text"
    request.setRequestHeader("Content-Type", "text/plain")
    requestBase64 match {
      case Some(base64) => request.send(base64)
      case None => request.send()
    }

    promise.future
  }

  /** URL encodes the given string */
  def encode(value: Any): String = URIUtils.encodeURIComponent(value.toString)

  /** Returns the server url */
  def hostPortString: String = {
    val uri = URI.create(window.location.href)
    uri.getPort match {
      case -1 => s"${uri.getScheme}://${uri.getHost}"
      case port => s"${uri.getScheme}://${uri.getHost}:$port"
    }
  }

  /** Returns the server url but with https */
  def httpsString: String = {
    val uri = URI.create(window.location.href)
    uri.getPort match {
      case -1 => s"https://${uri.getHost}"
      case port => s"https://${uri.getHost}:$port"
    }
  }

  /** Returns true if running locally */
  def isLocalhost: Boolean = hostPortString.contains("127.0.0.1")

  /** Returns true if protocol is secured */
  def isHttps: Boolean = hostPortString.startsWith("https://")

  /** Returns the path part of the url */
  def pathString: String = history.location.pathname

  /** Returns the path part with appended query string */
  def routeString: String = s"$pathString$queryString"

  /** Returns the full query string */
  def queryString: String = history.location.search

  /** Returns the query parameter map from current URL */
  def queryParameters: Map[String, List[String]] = uri.parseQuery(history.location.search)

  /** Returns the first value of a query parameter with given name */
  def queryParameter(name: String): Option[String] = {
    queryParameters.get(name).flatMap(values => values.headOption)
  }
}