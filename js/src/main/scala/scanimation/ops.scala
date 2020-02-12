package scanimation

import java.util.UUID

import lib.facade.pixi._
import org.querki.jquery._
import org.scalajs.dom
import org.scalajs.dom.raw.DOMList
import scanimation.common._
import scanimation.util.animation
import scanimation.util.animation.{Animation, ChaseInOut, Delay, FadeIn, FadeOut, FlipIn, FlipOut, OffsetIn, OffsetOut, Parallel}
import scanimation.util.global.GlobalContext
import scanimation.util.logging.Logging

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.util.{Failure, Success, Try}

//noinspection LanguageFeature
object ops extends GlobalContext with Logging {
  override protected def logKey: String = "ops"

  /** Converts scala map into javascript object */
  implicit def mapToJs[A](map: Map[String, A]): js.Dictionary[A] = map.toJSDictionary

  /** Converts scala traversable into javascript array */
  implicit def collectionToJsArray[A](list: Traversable[A]): js.Array[A] = list.toJSArray

  /** Converts points into vectors */
  implicit def pointToVec(point: Point): Vec2d = point.x xy point.y

  /** Converts integer vector in double vector */
  implicit def vec2iToVec2d(v: Vec2i): Vec2d = Vec2d(v.x, v.y)

  /** Converts rect2d into pixi rectangle */
  implicit def rect2dToRectangle(rect: Rec2d): Rectangle = new Rectangle(rect.position.x, rect.position.y, rect.size.x, rect.size.y)

  /** On error, prints it's stacktrace to the console */
  def unsafe[A](message: String)(code: => A): A = Try(code) match {
    case Success(a) => a
    case Failure(error) =>
      dom.console.error(message)
      error.printStackTrace()
      throw error
  }

  /** Builds a delay animation with given amount of time */
  def delay(time: FiniteDuration = animation.AnimationDelay): Animation = Delay(time)

  /** Randomizes the number between two given ones */
  def randomBetween(start: Double, end: Double): Double = start + Math.random() * (end - start)

  /** Applies the given code in next anumation frame */
  def nextFrame(code: => Unit): Unit = dom.window.requestAnimationFrame(_ => code)

  implicit class JsMapOps(val map: Map[String, js.Any]) extends AnyVal {
    /** Converts map to javascript object */
    def asJs: js.Dictionary[js.Any] = mapToJs(map)
  }

  implicit class DisplayObjectOps[A <: DisplayObject](val a: A) extends AnyVal {
    /** Randomizes the uuid of the object if not already assigned */
    def ensureUuid: A = a.mutate { a => if (!a.hasOwnProperty("uuid")) a.uuid = UUID.randomUUID().toString }

    /** Sets the z to -1 if not already assigned */
    def ensureZ: A = a.mutate { a => if (!a.hasOwnProperty("z")) a.z = -1 }

    /** Returns true if the object has not parent */
    def detached: Boolean = Option(a.parent).isEmpty

    /** Removes the object from it's parent */
    def detach: A = a.mutate { a => if (!detached) a.parent.removeChild(a) }

    /** Changes the width and height to the given size vector */
    def resizeTo(size: Vec2d): A = a.mutate { a =>
      a.width = size.x
      a.height = size.y
    }

    /** Changes the scale to the given value */
    def scaleTo(scale: Double): A = a.mutate { a => a.scale.set(scale, scale) }

    /** Changes the scale to the given value */
    def scaleTo(scale: Vec2d): A = a.mutate { a => a.scale.set(scale) }

    /** Changes the X scale to the given value */
    def scaleXTo(scale: Double): A = a.mutate { a => a.scale.x = scale }

    /** Changes the Y scale to the given value */
    def scaleYTo(scale: Double): A = a.mutate { a => a.scale.y = scale }

    /** Changes the X slew to the given value */
    def skewXTo(scale: Double): A = a.mutate { a => a.skew.x = scale }

    /** Changes the Y slew to the given value */
    def skewYTo(scale: Double): A = a.mutate { a => a.skew.y = scale }

    /** Changes the alpha to given value */
    def alphaAt(alpha: Double): A = a.mutate { a => a.alpha = alpha }

    /** Changes the position to the given location */
    def positionAt(position: Vec2d): A = a.mutate { a => a.position.set(position) }

    /** Changes the anchor location to the center of sprite */
    def anchorAtCenter: A = a.anchorAt(Vec2d.Center)

    /** Changes the anchor location to a given value */
    def anchorAt(anchor: Vec2d): A = a.mutate { a => a.anchor.set(anchor) }

    /** Changes the pivot location to a given value */
    def pivotAt(pivot: Vec2d): A = a.mutate { a => a.pivot.set(pivot) }

    /** Changes the rotation to a given value */
    def rotateTo(rotation: Double): A = a.mutate { a => a.rotation = rotation }

    /** Adds as a child to the given container */
    def addTo(parent: Container): A = a.mutate { a => parent.addChild(a) }

    /** Changes the visibility to a given value */
    def visibleTo(visible: Boolean): A = a.mutate { a => a.visible = visible }

    /** Changes the interactivity to a given value */
    def interactiveTo(interactive: Boolean): A = a.mutate { a => a.interactive = true }

    /** Adds the given object as mask */
    def maskWith(mask: => DisplayObject): A = a.mutate { a => a.mask = mask }

    /** Changes the filters of the object */
    def filterWith(filters: List[Filter]): A = a.mutate { a => a.filters = filters }

    /** Binds the scale to a given bind */
    def bindScale(data: Data[Double]): A = a.mutate { a => data /> { case scale => a.scaleTo(scale) } }

    /** Returns the absolute position of the object in the world */
    def absolutePosition: Vec2d = a.worldTransform.transform(m => m.tx xy m.ty)

    /** Returns the absolute rotation of the object as a sum of parent rotations */
    def absoluteRotation: Double = a.parentList.map(p => p.rotation).sum + a.rotation

    /** Returns the absolute scale of the object as a multiplication of parent scales */
    def absoluteScale: Vec2d = a.parentList.foldLeft(pointToVec(a.scale)) { case (scale, parent) => scale * parent.scale }

    /** Returns a list of all parents up to the root */
    def parentList: List[DisplayObject] = {
      @tailrec
      def rec(parents: List[DisplayObject], current: DisplayObject): List[DisplayObject] = Option(current.parent) match {
        case None => parents
        case Some(parent) => rec(parents :+ parent, parent)
      }

      rec(Nil, a)
    }

    /** Moves, scales and rotates the object to given anchor immediately */
    def warpTo(anchor: DisplayObject): A = a.mutate { a => a.positionAt(anchor.absolutePosition).scaleTo(anchor.absoluteScale).rotateTo(anchor.absoluteRotation) }

    def fadeIn: FadeIn = FadeIn(a)

    def fadeOut: FadeOut = FadeOut(a)

    def flipIn: FlipIn = FlipIn(a)

    def flipOut: FlipOut = FlipOut(a)

    def offsetIn(original: Vec2d, offset: Vec2d): OffsetIn = OffsetIn(a, original, offset)

    def offsetOut(original: Vec2d, offset: Vec2d): OffsetOut = OffsetOut(a, original, offset)

    def chase(source: DisplayObject, target: DisplayObject): ChaseInOut = ChaseInOut(a, source, target)
  }

  implicit class ContainerOps(val a: Container) extends AnyVal {
    /** Builds a new sub-container */
    def sub: Container = new Container().addTo(a)

    /** Removes all children from container */
    def removeChildren: Container = a.mutate { a => while (a.children.nonEmpty) a.removeChild(a.children.head) }
  }

  implicit class PointOps(val p: Point) extends AnyVal {
    /** Sets the point values to vector fields */
    def set(vec: Vec2d): Unit = p.set(vec.x, vec.y)
  }

  implicit class TextStyleOps(val style: TextStyle) extends AnyVal {
    /** Creates a text object from given style */
    def text(text: String): Text = new Text(text, style)
  }

  implicit class AnimationListOps(val list: List[Animation]) extends AnyVal {
    /** Executes animations in parallel */
    def parallel: Animation = Parallel(list)
  }

  implicit class DomListOps[A](val list: DOMList[A]) extends AnyVal {
    /** Converts DOM list into Scala list */
    def asList: List[A] = (0 until list.length).map(index => list.item(index)).toList
  }

  implicit class JqueryOps(val jquery: JQuery) extends AnyVal {
    /** Reads the first jquery element as DOM element of type A */
    def firstAs[A]: A = jquery.get(0).asInstanceOf[A]

    /** Toggles the visibility in a safe way, better than show/hide */
    def visible(flag: Boolean = true): JQuery = {
      jquery.each { element =>
        val one = $(element)
        val cssDisplay = Option(one.css("display")).filter(v => v.nonEmpty).getOrElse("initial")
        if (flag) {
          if (cssDisplay == "none") {
            val storedDisplay = one.data("display").map(v => v.toString).getOrElse("initial")
            one.css("display", storedDisplay)
          }
        } else {
          if (cssDisplay != "none") {
            one.data("display", cssDisplay)
            one.hide()
          }
        }
      }
    }

    /** Hides the element in a safe way, better than hide */
    def hidden(): JQuery = visible(false)

    /** Enables the button */
    def enable(flag: Boolean = true): JQuery = jquery.prop("disabled", !flag)

    /** Disables the button */
    def disable(): JQuery = jquery.enable(false)

    /** Sets the list id to given value */
    def id(id: ListElementId): JQuery = jquery.attr("data-id", id)
  }

  implicit class WriteableOps[A](val model: Writeable[A]) extends AnyVal {
    /** Binds the model to dropdown */
    def bindDropdown(jquery: JQuery, viewCode: A => String, modelCode: String => A, controllerCode: A => Unit): JQuery = {
      model /> { case next =>
        val last = jquery.value().toString
        val nextString = viewCode.apply(next)
        if (last != nextString) jquery.value(nextString)
      }
      jquery.change(() => {
        val next = modelCode.apply(jquery.value().toString)
        val last = model.read
        if (next != last) controllerCode.apply(next)
      })
    }
  }

  implicit class WriteableOptionOps[A](val model: Writeable[Option[A]]) extends AnyVal {
    /** Binds the model to text field with validation, adds the "bad" class to the text field when validation fails */
    def bindValidatedText(jquery: JQuery, viewCode: A => String, modelCode: String => A, validationCode: A => Boolean, controllerCode: Option[A] => Unit): JQuery = {
      model /> {
        case Some(next) =>
          val last = jquery.value().toString
          val nextString = viewCode.apply(next)
          if (last != nextString) jquery.value(nextString)
          jquery.removeClass("bad")
      }
      jquery.on("input", () => {
        val nextString = jquery.value().toString
        val last = model.read
        Try(modelCode.apply(nextString))
          .toOption
          .filter(v => validationCode.apply(v)) match {
          case Some(next) if next != last =>
            controllerCode.apply(Some(next))
            jquery.removeClass("bad")
          case _ =>
            controllerCode.apply(None)
            jquery.addClass("bad")
        }
      })
    }
  }

  implicit class ListElementIdOps(val id: ListElementId) extends AnyVal {
    /** Selects all elements with data id equal to give string */
    def item: JQuery = $(s"""[data-id="$id"]""")
  }

}