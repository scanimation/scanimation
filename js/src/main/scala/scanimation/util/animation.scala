package scanimation.util

import java.util.UUID

import lib.facade.pixi.DisplayObject
import scanimation.common._
import scanimation.mvc.Controller
import scanimation.ops._
import scanimation.util.global.GlobalContext

import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

object animation extends GlobalContext {
  val AnimationDelay: FiniteDuration = 300.millis
  private var queue: Future[Unit] = UnitFuture
  private var current: Option[Player] = None
  private var time: Long = System.currentTimeMillis()

  /** Loads the animation loop */
  def load()(implicit controller: Controller): Future[Unit] = Future {
    controller.model.tick /> { case _ => update() }
  }

  /** Plays the next animation tick */
  private def update(): Unit = this.synchronized {
    val delta = System.currentTimeMillis() - time
    time = time + delta
    current = current.flatMap { player =>
      val currTime = player.time + delta
      val totalTime = player.animation.duration.toMillis
      if (currTime >= totalTime) {
        player.animation.finish()
        player.promise.success()
        None
      } else {
        val position = player.animation.ease.apply(currTime.toDouble / totalTime)
        player.animation.update(position)
        Some(player.copy(time = currTime))
      }
    }
  }

  /** Adds the list of parallel animations to the queue */
  def append(anim: Animation): Future[Unit] = this.synchronized {
    queue = queue.flatMap { _ =>
      val player = Player(anim)
      this.synchronized {
        anim.start()
        current = Some(player)
      }
      player.promise.future
    }
    queue
  }

  /** Adds an animation to the queue */
  def +=(anim: Animation): this.type = {
    append(anim)
    animation
  }

  /** Adds a list of parallel animations to the queue */
  def ++=(anims: List[Animation]): this.type = {
    append(Parallel(anims))
    animation
  }

  /** Represents an animation with some time length and easing function */
  sealed trait Animation {
    /** Unique identifier for the animation */
    lazy val uuid: String = UUID.randomUUID().toString

    /** Is called when animation first starts */
    def start(): Unit = update(0)

    /** Is called every tick with value varying from 0 to 1 according to current time + animation ease */
    def update(value: Double): Unit

    /** Is called when animation reaches the time end */
    def finish(): Unit = update(1)

    /** Defines the easing function for the animation */
    def ease: Ease

    /** Defines the animation duration */
    def duration: FiniteDuration

    /** Chains the two animations together to be played in sequence */
    def chain(next: Animation): Chain = Chain(this :: next :: Nil)

    /** Chains the animation with a group of other animations that should play in parallel */
    def chain(group: List[Animation]): Chain = Chain(this :: Parallel(group) :: Nil)

    /** Triggers the given code on animation start */
    def onStart(code: => Unit): Animation = OnStartWrapper(this, { () => code })

    /** Triggers the given code on animation end */
    def onEnd(code: => Unit): Animation = OnEndWrapper(this, { () => code })

    override def toString: String = s"${getClass.getSimpleName}:$uuid"
  }

  /** Represents a utility zero time animation */
  object EmptyAnimation extends Animation {
    override def update(value: Double): Unit = {}

    override def ease: Ease = LinearEase

    override def duration: FiniteDuration = 0.millis
  }

  /** Wraps the animation and triggers code at it's start */
  case class OnStartWrapper(wrap: Animation, code: () => Unit) extends Animation {
    override def start(): Unit = {
      code.apply()
      wrap.start()
    }

    override def update(value: Double): Unit = wrap.update(value)

    override def finish(): Unit = wrap.finish()

    override def ease: Ease = wrap.ease

    override def duration: FiniteDuration = wrap.duration
  }

  /** Wraps the animation and triggers code at it's end */
  case class OnEndWrapper(wrap: Animation, code: () => Unit) extends Animation {
    override def start(): Unit = wrap.start()

    override def update(value: Double): Unit = wrap.update(value)

    override def finish(): Unit = {
      code.apply()
      wrap.finish()
    }

    override def ease: Ease = wrap.ease

    override def duration: FiniteDuration = wrap.duration
  }

  /** Simple time sleep animation */
  case class Delay(duration: FiniteDuration = AnimationDelay) extends Animation {
    override def update(value: Double): Unit = {}

    override def ease: Ease = LinearEase
  }

  /** A chain of animations played one by one */
  case class Chain(animations: List[Animation]) extends Animation {
    private var schedule: List[(Long, Animation)] = animations.zipWithIndex.map { case (anim, index) => animations.take(index).map(a => a.duration.toMillis).sum -> anim }
    private var current: Option[(Long, Animation)] = None

    override def start(): Unit = {
      current = schedule.headOption
      schedule = schedule.drop(1)
      current.foreach { case (s, a) => a.start() }
    }

    override def update(value: Double): Unit = {
      val time = value * duration.toMillis
      val (reached, future) = schedule.partition { case (start, anim) => time >= start }
      reached.foreach { case (start, anim) =>
        current.foreach { case (s, a) => a.finish() }
        current = Some(start -> anim)
        anim.start()
      }
      current.foreach { case (start, anim) =>
        val local = time - start
        val position = anim.ease.apply(local / anim.duration.toMillis) min 1.0
        anim.update(position)
      }
      schedule = future
    }

    override def finish(): Unit = {
      current.foreach { case (s, a) => a.finish() }
      schedule.foreach { case (s, a) => a.finish() }
    }

    override val ease: Ease = LinearEase

    override val duration: FiniteDuration = animations.map(a => a.duration.toMillis).sum.millis

    override def chain(next: Animation): Chain = this.copy(animations = animations :+ next)

    override def chain(group: List[Animation]): Chain = this.copy(animations = animations :+ Parallel(group))
  }

  /** A group of parallel animations played together */
  case class Parallel(animations: List[Animation]) extends Animation {
    private var current: List[Animation] = animations

    override def start(): Unit = {
      animations.foreach(a => a.start())
    }

    override def update(value: Double): Unit = {
      val currTime = value * duration.toMillis
      current = current.flatMap { anim =>
        val totalTime = anim.duration.toMillis
        if (currTime >= totalTime) {
          anim.finish()
          None
        } else {
          val position = anim.ease.apply(currTime / totalTime)
          anim.update(position)
          Some(anim)
        }
      }
    }

    override val ease: Ease = LinearEase

    override val duration: FiniteDuration = if (animations.nonEmpty) animations.map(a => a.duration.toMillis).max.millis else 0.millis
  }

  /** Flips the object starting from edge position, ending with rest position */
  case class FlipIn(o: DisplayObject, ease: Ease = EaseOut, duration: FiniteDuration = AnimationDelay) extends Animation {
    override def update(value: Double): Unit = o.scaleXTo(value)
  }

  /** Flips the object starting from resting position, ending with edge position */
  case class FlipOut(o: DisplayObject, ease: Ease = EaseIn, duration: FiniteDuration = AnimationDelay) extends Animation {
    override def update(value: Double): Unit = o.scaleXTo(1 - value)
  }

  /** Fades the object starting with 0 alpha, ending with 1 alpha */
  case class FadeIn(o: DisplayObject, ease: Ease = LinearEase, duration: FiniteDuration = AnimationDelay) extends Animation {
    override def update(value: Double): Unit = o.alphaAt(value)
  }

  /** Fades the object starting with 1 alpha, ending with 0 alpha */
  case class FadeOut(o: DisplayObject, ease: Ease = LinearEase, duration: FiniteDuration = AnimationDelay) extends Animation {
    override def update(value: Double): Unit = o.alphaAt(1 - value)
  }

  /** Moves the object into original position from the offset */
  case class OffsetIn(o: DisplayObject, original: Vec2d, offset: Vec2d, ease: Ease = EaseOut, duration: FiniteDuration = AnimationDelay) extends Animation {
    override def update(value: Double): Unit = o.positionAt(original + offset * (1 - value))
  }

  /** Moves the object from original position into the offset */
  case class OffsetOut(o: DisplayObject, original: Vec2d, offset: Vec2d, ease: Ease = EaseIn, duration: FiniteDuration = AnimationDelay) extends Animation {
    override def update(value: Double): Unit = o.positionAt(original + offset * value)
  }

  /** Moves, rotates, scales the object from the source to the target anchor */
  case class ChaseInOut(o: DisplayObject, source: DisplayObject, target: DisplayObject, ease: Ease = EaseInOut, duration: FiniteDuration = AnimationDelay) extends Animation {
    override def update(value: Double): Unit = {
      o
        .positionAt((source.absolutePosition, target.absolutePosition) %% value)
        .scaleTo((source.absoluteScale, target.absoluteScale) %% value)
        .rotateTo((source.absoluteRotation, target.absoluteRotation).rotationProgress(value))
    }
  }

  /** Internal tracker of the played animation */
  private case class Player(animation: Animation, time: Long = 0, promise: Promise[Unit] = Promise[Unit])

  /** Defines the easing function to be used for animations */
  sealed trait Ease {
    def apply(x: Double): Double
  }

  /** Starts slow, ends fast */
  object EaseIn extends Ease {
    override def apply(x: Double): Double = x * x
  }

  /** Starts fast, ends slow */
  object EaseOut extends Ease {
    override def apply(x: Double): Double = x * (2 - x)
  }

  /** Starts slow, ends slow */
  object EaseInOut extends Ease {
    override def apply(x: Double): Double = if (x < 0.5) 2 * x * x else -2 * x * x + 4 * x - 1
  }

  /** Gradually moves towars the end */
  object LinearEase extends Ease {
    override def apply(x: Double): Double = x
  }

}