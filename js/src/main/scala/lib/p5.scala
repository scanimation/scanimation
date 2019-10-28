package lib

import lib.facade.p5.AudioIn
import scanimation.common.ListenerId
import scanimation.mvc.Controller
import scanimation.util.global.GlobalContext
import scanimation.util.logging.Logging
import org.scalajs.dom.raw.AudioContext

import scala.concurrent.{Future, Promise}

/** p5.js is a JavaScript library for creative coding, with a focus on making coding accessible and inclusive for artists, designers, educators, beginners, and anyone else!
  * p5.js is free and open-source because we believe software, and the tools to learn it, should be accessible to everyone.
  *
  * https://p5js.org/
  */
object p5 extends GlobalContext with Logging {
  override protected def logKey: String = "p5"

  /** Creates microphone input */
  def audioIn(controller: Controller): Future[AudioIn] = {
    implicit val listenerId: ListenerId = ListenerId()
    val promise = Promise[AudioIn]()
    val mic = new AudioIn({ () =>
      log.warn("AudioIn constructor error called")
      promise.tryFailure(new IllegalStateException("Failed to create AudioIn"))
    })
    mic.start(
      successCallback = { () => log.info("AudioIn start success called") },
      errorCallback = { () =>
        log.warn("AudioIn start error called")
        promise.tryFailure(new IllegalStateException("Failed to start AudioIn"))
      }
    )
    controller.model.tick /> { case _ =>
      if (mic.enabled) {
        log.info("AudioIn microphone prompt accepted")
        controller.model.tick.forget()
        promise.trySuccess(mic)
      }
    }
    promise.future
  }

  /** Returns root audio context */
  def audioContext: AudioContext = lib.facade.p5.p5.getAudioContext()
}