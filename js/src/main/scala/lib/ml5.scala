package lib

import lib.facade.ml5.PitchDetection
import lib.facade.p5.AudioIn
import scanimation.util.global.GlobalContext
import org.scalajs.dom.raw.AudioContext

import scala.concurrent.{Future, Promise}
import scala.scalajs.js.UndefOr

/** Friendly Machine Learning for the Web
  * https://ml5js.org/
  */
object ml5 extends GlobalContext {
  /** Creates a pitch detector from given model
    *
    * @param modelPath the server path to model files
    * @param mic       the started p5 audio source
    * @param context   the web audio context
    * @return the pitch detector to be used for further calls
    */
  def pitchDetection(modelPath: String, mic: AudioIn, context: AudioContext): Future[PitchDetection] = {
    val promise = Promise[PitchDetection]()
    ec.execute(() => {
      val pd = lib.facade.ml5.ml5.pitchDetection(modelPath, context, mic.stream, { () => promise.tryFailure(new IllegalStateException(s"Failed to load the model: $modelPath")) })
      promise.trySuccess(pd)
    })
    promise.future
  }

  implicit class PitchDetectionOps(val pd: PitchDetection) extends AnyVal {
    /** Returns the currently detected frequency */
    def currentFrequency: Option[Double] = Option(pd.frequency).flatMap(f => f.toOption)

    /** Registers a detection consumer and starts detecting the pitch */
    def register(code: Option[Double] => Unit): Unit = {
      def detectOnce(error: UndefOr[Double], frequency: UndefOr[Double]): Unit = {
        code.apply(Option(frequency).flatMap(f => f.toOption))
        pd.getPitch(detectOnce)
      }

      pd.getPitch(detectOnce)
    }
  }

}