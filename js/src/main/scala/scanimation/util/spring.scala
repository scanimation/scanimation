package scanimation.util

import java.util.UUID
import java.util.concurrent.ConcurrentLinkedQueue

import lib.facade.pixi._
import scanimation.common._
import scanimation.mvc.Controller
import scanimation.ops._
import scanimation.util.global.GlobalContext
import scanimation.util.logging.Logging

import scala.concurrent.Future

object spring extends GlobalContext with Logging {
  override protected def logKey: String = "spring"

  private val queue: ConcurrentLinkedQueue[Mutator] = new ConcurrentLinkedQueue[Mutator]()
  private var updaters: List[Updater] = Nil

  /** Adds the updater into the loop */
  def add[A <: Updater](updater: A): A = {
    queue.add(AddUpdater(updater))
    updater
  }

  /** Removes the updater from the loop */
  def remove[A <: Updater](updater: A): A = {
    queue.add(RemoveUpdater(updater))
    updater
  }

  /** Loads the update loop */
  def load()(implicit controller: Controller): Future[Unit] = Future {
    log.info("starting...")
    controller.model.tick /> { case tick => update() }
    log.info("is running")
  }

  /** Fires a tick of the updates  */
  def update(): Unit = this.synchronized {
    Stream.from(0)
      .map(_ => Option(queue.poll()))
      .takeWhile(mutatorOpt => mutatorOpt.isDefined)
      .flatten
      .foreach {
        case AddUpdater(updater) =>
          updaters = updaters :+ updater
        case RemoveUpdater(updater) =>
          updaters = updaters.filterNot {
            case remove if remove.uuid == updater.uuid => true
            case _ => false
          }
      }
    updaters.foreach(u => u.update())
  }

  trait Updater {
    protected val randomId: String = UUID.randomUUID().toString

    /** Uniquely identifies the updater */
    def uuid: String

    /** Updates the current value to the next one */
    def update(): Unit
  }

  case class SpritePositionSpring(sprite: DisplayObject, var target: Vec2d = Vec2d.Zero, var speed: Double = 0.5) extends Updater {
    override def uuid: String = s"position-spring-${sprite.ensureUuid.uuid}"

    override def update(): Unit = {
      sprite.positionAt((sprite.position: Vec2d, target) %% speed)
    }
  }

  case class SpriteAnchorSpring(sprite: DisplayObject, var anchor: Container, var enabled: Boolean = true, var speed: Double = 0.5) extends Updater {
    override def uuid: String = s"anchor-spring-${sprite.ensureUuid.uuid}"

    override def update(): Unit = if (enabled) {
      val position = (sprite.position: Vec2d, anchor.absolutePosition) %% speed
      val rotation = (sprite.rotation, anchor.absoluteRotation).rotationProgress(speed)
      val scale = (sprite.scale: Vec2d, anchor.absoluteScale) %% speed
      sprite.positionAt(position)
      sprite.rotateTo(rotation)
      sprite.scaleTo(scale)
    }
  }

  case class DoubleSpring(var current: Double, var target: Double, handler: DoubleSpring => Unit, var speed: Double = 0.5) extends Updater {
    override val uuid: String = s"double-spring-$randomId"

    override def update(): Unit = {
      current = (current, target) %% speed
      handler.apply(this)
    }
  }

  /** Message that mutates the loop state during the next tick */
  sealed trait Mutator

  case class AddUpdater(updater: Updater) extends Mutator

  case class RemoveUpdater(updater: Updater) extends Mutator

}