package scanimation

import java.lang.System.currentTimeMillis
import java.time.OffsetDateTime
import java.util.UUID

import scanimation.common.Transition.{Failed, Loaded, Loading, Missing, TransitionException}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success}

object common {

  /** Returns the empty future to start for comprehensions */
  val UnitFuture: Future[Unit] = Future.successful()

  /** Returns random uuid */
  def uuid: String = UUID.randomUUID().toString

  implicit class AnyOps[A](val a: A) extends AnyVal {
    /** Chains the execution to the given code block */
    def chain[B](code: A => B): B = code.apply(a)

    /** Chains the execution if condition is true */
    def chainIf(condition: Boolean)(code: A => A): A = if (condition) code.apply(a) else a

    /** Executes the given code and returns the initial unmodified value */
    def validate(code: A => Unit): A = {
      code.apply(a)
      a
    }

    /** Chains the call for some value */
    def chainOpt[B](opt: Option[B])(code: (A, B) => A): A = opt match {
      case Some(b) => code.apply(a, b)
      case None => a
    }

    /** Calls the mutating code */
    def mutate(code: A => Unit): A = {
      code.apply(a)
      a
    }

    /** Calls the mutating code for some value */
    def mutateOpt[B](opt: Option[B])(code: (A, B) => Unit): A = opt match {
      case Some(b) => code.apply(a, b); a
      case None => a
    }

    /** Transforms the value with given code */
    def transform[B](code: A => B): B = code.apply(a)

    /** Wraps the value into the future */
    def future: Future[A] = Future.successful(a)
  }

  implicit class DoubleOps(val double: Double) extends AnyVal {
    /** Formats the given double with given number of digits after point */
    def pretty(digits: Int = 2): String = String.format(s"%.${digits}f", double: java.lang.Double)

    /** Normalizes the rotation by removing extra PIs */
    def normalRotation: Double = {
      if (double > Math.PI || double < -Math.PI) double - Math.PI * (double / Math.PI).toInt
      else double
    }

    /** Converts value to radians */
    def rad: Double = double / 180 * Math.PI

    /** Calculates log of the value */
    def log: Double = Math.log(double)
  }

  implicit class DoubleTupleOps(val tuple: (Double, Double)) extends AnyVal {
    /** Returns a number between two components according to progress */
    def %%(progress: Double): Double = tuple._1 + (tuple._2 - tuple._1) * progress

    /** Returns a number either at the end of the range or between the range */
    def %%%(range: (Double, Double), current: Double): Double = {
      if (current <= range._1) {
        tuple._1
      } else if (current >= range._2) {
        tuple._2
      } else {
        tuple %% ((current - range._1) / (range._2 - range._1))
      }
    }

    /** Returns a rotation between two components according to progress, normalizes the components */
    def rotationProgress(progress: Double): Double = {
      val (a, b) = tuple
      val (an, bn) = (a.normalRotation, b.normalRotation)
      val diff = List(bn - an, bn + Math.PI * 2 - an, bn - Math.PI * 2 - an).minBy(diff => diff.abs)
      (a, a + diff) %% progress
    }
  }

  implicit class TraversableOps[A](val list: Traversable[A]) extends AnyVal {
    /** Safely calculates min for non-empty lists */
    def minOpt(implicit ordering: Ordering[A]): Option[A] = list match {
      case empty if empty.isEmpty => None
      case values => Some(values.min)
    }

    /** Safely calculates minBy for non-empty lists */
    def minByOpt[B](code: A => B)(implicit ordering: Ordering[B]): Option[A] = list match {
      case empty if empty.isEmpty => None
      case values => Some(values.minBy(code))
    }

    /** Safely calculates max for non-empty lists */
    def maxOpt(implicit ordering: Ordering[A]): Option[A] = list match {
      case empty if empty.isEmpty => None
      case values => Some(values.max)
    }

    /** Safely calculates maxBy for non-empty lists */
    def maxByOpt[B](code: A => B)(implicit ordering: Ordering[B]): Option[A] = list match {
      case empty if empty.isEmpty => None
      case values => Some(values.maxBy(code))
    }

    /** Safely calculates min or returns default value for empty list */
    def minOr(default: A)(implicit ordering: Ordering[A]): A = list.minOpt.getOrElse(default)

    /** Safely calculates max or returns default value for empty list */
    def maxOr(default: A)(implicit ordering: Ordering[A]): A = list.maxOpt.getOrElse(default)
  }

  implicit class MapOps[A, B](val map: Map[A, B]) extends AnyVal {
    /** Ensures that value exists in mutable map reference */
    def ensure(key: A, rewrite: Map[A, B] => Unit)(code: => B): B = {
      map.get(key) match {
        case Some(value) => value
        case None =>
          val value = code
          rewrite.apply(map + (key -> value))
          value
      }
    }
  }

  implicit class RandomOps(val random: Random) extends AnyVal {
    /** Returns a random element from the sequence */
    def oneOf[A](list: List[A]): A = list(random.nextInt(list.size))

    /** Returns a subset of random element from the given seq */
    def multipleOf[A](list: List[A], size: Int): List[A] = size match {
      case tooMuch if size > list.size => throw new IllegalArgumentException(s"random seq size must not be greater than original seq size: expected <= ${list.size}, got $size")
      case sameSize if size == list.size => random.shuffle(list)
      case other => random.shuffle(list).take(other)
    }

    /** Returns a number between start and end inclusive */
    def between(start: Int, end: Int): Int = {
      random.nextInt(end - start + 1) + start
    }
  }

  implicit class ListVec2iOps(val list: Traversable[Vec2i]) extends AnyVal {
    /** Returns the min to max range of X values */
    def rangeX: Option[Vec2i] = list match {
      case empty if empty.isEmpty => None
      case nonempty =>
        val xs = list.map(v => v.x)
        Some(xs.min xy xs.max)
    }

    /** Returns the min to max range of Y values */
    def rangeY: Option[Vec2i] = list match {
      case empty if empty.isEmpty => None
      case nonempty =>
        val ys = list.map(v => v.y)
        Some(ys.min xy ys.max)
    }
  }

  implicit class FiniteDurationOps(val duration: FiniteDuration) extends AnyVal {
    /** Multiplies the duration by given number */
    def **(multiplier: Double): FiniteDuration = (duration.toMillis * multiplier).toLong.millis

    /** Returns formatted duration: "1d2h3m4s5ms" */
    def prettyString: String = {
      val (ignored, stringified) = List(
        1.day -> "d",
        1.hour -> "h",
        1.minute -> "m",
        1.second -> "s",
        1.millis -> "ms"
      ).foldLeft(duration, "") { case ((left, string), (unitDuration, unitName)) =>
        val amount = left.toMillis / unitDuration.toMillis
        if (amount > 0) {
          (left - amount * unitDuration, s"$string$amount$unitName")
        } else {
          (left, string)
        }
      }
      stringified
    }
  }

  type TransitionListener[A] = PartialFunction[(A, A), Unit]
  type EndListener[A] = PartialFunction[A, Unit]

  /** Wraps the mutable value for subscriptions */
  trait Data[A] {
    /** Returns the latest value */
    def read: A

    /** Adds the given listener that will be invoked on every update */
    def listen(code: TransitionListener[A], initialize: Boolean)(implicit listenerId: ListenerId = ListenerId()): Data[A]

    /** Forgets the given listener */
    def forget()(implicit listenerId: ListenerId): Data[A]

    /** Partially projects the wrapped value with given code */
    def partialMap[B](code: PartialFunction[A, B])(implicit listenerId: ListenerId = ListenerId()): Data[Option[B]]

    /** Maps the wrapped value with given code */
    def map[B](code: A => B)(implicit listenerId: ListenerId = ListenerId()): Data[B]

    /** Combines the wrapper with another one for simultaneous subscriptions */
    def and[B](other: Data[B])(implicit listenerId: ListenerId = ListenerId()): Data[(A, B)]


    /** Returns the latest value */
    def apply(): A = this.read

    /** Adds the given listener that will be invoked on every update */
    def />>(code: TransitionListener[A])(implicit listenerId: ListenerId = ListenerId()): Data[A] = {
      this.listen(code, initialize = true)
    }

    /** Adds the given listener that will be invoked on every update */
    def />(code: EndListener[A])(implicit listenerId: ListenerId = ListenerId()): Data[A] = {
      val listener: TransitionListener[A] = {
        case (before, after) if code.isDefinedAt(after) => code.apply(after)
      }
      this.listen(listener, initialize = true)
    }

    /** Partially projects the wrapped value with given code */
    def /~[B](code: PartialFunction[A, B])(implicit listenerId: ListenerId = ListenerId()): Data[Option[B]] = this.partialMap(code)

    /** Combines the wrapper with another one for simultaneous subscriptions */
    def &&[B](other: Data[B])(implicit listenerId: ListenerId = ListenerId()): Data[(A, B)] = this.and(other)

    /** Forcibly triggers the listeners */
    def forceTrigger(): Unit

    override def toString: String = s"$read"
  }

  /** Represents the wrapper that can mutate it's value */
  trait Writeable[A] extends Data[A] {
    /** Mutates the current value into a given one */
    def write(a: A): A
  }

  /** The unique identifier for the data listener */
  case class ListenerId(value: String = uuid)

  private class Implementation[A](default: A, skipDuplicates: Boolean) extends Writeable[A] {
    private var value: A = default
    private var listeners: List[(ListenerId, TransitionListener[A])] = Nil
    private var subData: List[Data[_]] = Nil

    override def partialMap[B](code: PartialFunction[A, B])(implicit listenerId: ListenerId = ListenerId()): Data[Option[B]] = {
      new Implementation[Option[B]](code.lift.apply(value), skipDuplicates).mutate { source =>
        this.listen({ case (before, after) =>
          val current = source.read
          val next = code.lift.apply(after)
          if (current != next) source.write(next)
        }, initialize = true)
      }
    }

    override def read: A = value

    override def map[B](code: A => B)(implicit listenerId: ListenerId = ListenerId()): Data[B] = {
      new Implementation(code.apply(value), skipDuplicates).mutate { source =>
        this.listen({ case (before, after) => source.write(code.apply(after)) }, initialize = true)
        subData = source :: subData
      }
    }

    override def write(a: A): A = this.synchronized {
      val before = value
      val after = a
      value = after
      if (skipDuplicates && before == after) {
        // skip
      } else {
        listeners.foreach { case (id, listener) => listener.lift.apply(before, after) }
      }
      value
    }

    override def listen(code: TransitionListener[A], initialize: Boolean)(implicit listenerId: ListenerId = ListenerId()): Data[A] = this.synchronized {
      listeners = listeners :+ (listenerId -> code)
      if (initialize) {
        code.lift.apply(value, value)
      }
      this
    }

    override def forget()(implicit listenerId: ListenerId): Data[A] = {
      listeners = listeners.filterNot { case (id, listener) => id == listenerId }
      subData.foreach(sub => sub.forget())
      this
    }

    override def and[B](other: Data[B])(implicit listenerId: ListenerId = ListenerId()): Data[(A, B)] = {
      new Implementation((this.read, other.read), skipDuplicates)
        .mutate { source =>
          val handler: TransitionListener[Any] = source.synchronized {
            case _ => source.write(this.read, other.read)
          }

          this.listen(handler, initialize = true)
          other.listen(handler, initialize = true)
          subData = source :: subData
        }
    }

    override def forceTrigger: Unit = listeners.foreach { case (id, listener) => listener.lift.apply(value, value) }

    override def toString: String = value.toString
  }

  object Data {
    /** Creates the writable data source */
    def apply[A](default: A): Writeable[A] = new Implementation(default, skipDuplicates = false)
  }

  object LazyData {
    /** Creates the writable data source */
    def apply[A](default: A): Writeable[A] = new Implementation(default, skipDuplicates = true)
  }

  /** Defines the identifier of list elements */
  type ListElementId = String

  /** Wraps a mutable lists for subscriptions */
  class ListData[A] {
    /** Internal value map */
    private var map: Map[ListElementId, A] = Map.empty
    /** Internal index list */
    private var list: List[ListElementId] = Nil
    /** Internal selection set */
    private var selection: Set[ListElementId] = Set.empty

    /** Data representation of underlying list */
    val data: Writeable[List[A]] = LazyData(Nil)

    /** Handlers for change events */
    private var changeListeners: List[EndListener[List[A]]] = Nil
    /** Handlers for add events */
    private var addListeners: List[EndListener[(ListElementId, A)]] = Nil
    /** Handlers for remove events */
    private var removeListeners: List[EndListener[(ListElementId, A)]] = Nil
    /** Handlers for order events */
    private var orderListeners: List[EndListener[List[ListElementId]]] = Nil
    /** Handlers for select events */
    private var selectionListeners: List[EndListener[Set[ListElementId]]] = Nil

    /** Reads the list data for the given element */
    def read(id: ListElementId): Option[A] = map.get(id)

    /** Returns the current list of element ids */
    def ids: List[ListElementId] = list

    /** Returns the index of list data item within the list */
    def indexOf(id: ListElementId): Int = list.indexOf(id)

    /** Adds a list element at the end */
    def add(value: A): ListElementId = this.synchronized {
      val id = uuid
      map = map + (id -> value)
      list = list :+ id
      addListeners.foreach(l => l.lift.apply(id, value))
      notifyChanged()
      id
    }

    /** Appends a list of elements at the end */
    def addList(values: List[A]): List[ListElementId] = this.synchronized {
      values.map(v => add(v))
    }

    /** Removes the list element with given id */
    def remove(id: ListElementId): Unit = this.synchronized {
      map.get(id).foreach { a =>
        removeFromSelection(id)
        map = map - id
        list = list.without(id)
        removeListeners.foreach(l => l.lift.apply(id, a))
        notifyChanged()
      }
    }

    /** Empties the list data */
    def clear(): Unit = this.synchronized {
      if (list.nonEmpty) {
        deselectAll()
        val prev = map
        map = Map.empty
        list = Nil
        prev.foreach { case (id, a) => removeListeners.foreach(l => l.lift.apply(id, a)) }
        notifyChanged()
      }
    }

    /** Shifts given element up on the list */
    def moveUp(id: ListElementId): Unit = this.synchronized {
      list.indexOf(id) match {
        case -1 => // ignore
        case 0 => // ignore
        case index =>
          list = list.take(index - 1) ++ list.lift(index) ++ list.lift(index - 1) ++ list.drop(index + 1)
          orderListeners.foreach(l => l.lift.apply(list))
          notifyChanged()
      }
    }

    /** Shifts given element down on the list */
    def moveDown(id: ListElementId): Unit = this.synchronized {
      list.indexOf(id) match {
        case -1 => // ignore
        case last if last == list.size - 1 => // ignore
        case index =>
          list = list.take(index) ++ list.lift(index + 1) ++ list.lift(index) ++ list.drop(index + 2)
          orderListeners.foreach(l => l.lift.apply(list))
          notifyChanged()
      }
    }

    /** Sets the selection to a given element */
    def select(id: ListElementId): Unit = this.synchronized {
      if (selection != Set(id)) {
        deselectAll()
        addToSelection(id)
      }
    }

    /** Appends the given element to selection */
    def addToSelection(id: ListElementId): Unit = this.synchronized {
      if (!selection.contains(id)) {
        selection = selection + id
        selectionListeners.foreach(l => l.lift.apply(selection))
      }
    }

    /** Removes the given element from selection */
    def removeFromSelection(id: ListElementId): Unit = this.synchronized {
      if (selection.contains(id)) {
        selection = selection - id
        selectionListeners.foreach(l => l.lift.apply(selection))
      }
    }

    /** Clears the selection */
    def deselectAll(): Unit = this.synchronized {
      if (selection.nonEmpty) {
        selection = Set.empty
        selectionListeners.foreach(l => l.lift.apply(selection))
      }
    }

    /** Notifies about any list changes */
    private def notifyChanged(): Unit = {
      data.write(list.map(id => map(id)))
    }

    /** Starts listening to element addition events */
    def onAdd(listener: EndListener[(ListElementId, A)]): this.type = this.synchronized {
      addListeners = addListeners :+ listener
      this
    }

    /** Starts listening to element removal events */
    def onRemove(listener: EndListener[(ListElementId, A)]): this.type = this.synchronized {
      removeListeners = removeListeners :+ listener
      this
    }

    /** Starts listening to element reordering events */
    def onOrder(listener: EndListener[List[ListElementId]]): this.type = this.synchronized {
      orderListeners = orderListeners :+ listener
      this
    }

    /** Starts listening to element selection events */
    def onSelect(listener: EndListener[Set[ListElementId]]): this.type = this.synchronized {
      selectionListeners = selectionListeners :+ listener
      this
    }
  }

  object ListData {
    /** Creates an empty list data */
    def apply[A](): ListData[A] = new ListData[A]()
  }

  /** The view of single list data element */
  case class ListDataElementView[A](id: ListElementId, data: ListData[A]) {
    /** Returns the value of data element */
    def read(): A = data.read(id).get

    /** Removes itself from the list */
    def remove(): Unit = data.remove(id)

    /** Moves itself up in the list */
    def moveUp(): Unit = data.moveUp(id)

    /** Moves itself down in the list */
    def moveDown(): Unit = data.moveDown(id)

    /** Deselects all other elements and selects itself */
    def select(): Unit = data.select(id)
  }

  implicit class DataOptionOps[A](val data: Writeable[Option[A]]) extends AnyVal {
    /** Sets optional value to Some given value */
    def writeSome(a: A): Unit = data.write(Some(a))

    /** Updates the optional value, if present */
    def update(code: A => A): Unit = {
      val current = data.read
      val next = current.map(code)
      next.foreach(v => data.write(Some(v)))
    }
  }

  implicit class DataOptionOptionOps[A](val data: Data[Option[Option[A]]]) extends AnyVal {
    /** Unwraps the inner option for double option data */
    def flatten: Data[Option[A]] = data /~ { case Some(Some(a)) => a }
  }

  implicit class FutureOps[A](val future: Future[A]) extends AnyVal {
    /** Clears the future value into unit */
    def clear(implicit ec: ExecutionContext): Future[Unit] = future.map(a => ())

    /** Appends the next future without data dependency */
    def >>[B](other: Future[B])(implicit ec: ExecutionContext): Future[Unit] = future.flatMap(any => other).clear

    /** Executes given code when future is successful */
    def whenSuccessful(code: A => Unit)(implicit ec: ExecutionContext): Future[A] = {
      future.onComplete {
        case Success(value) => code.apply(value)
        case Failure(NonFatal(_)) => // ignore
      }
      future
    }

    /** Executes given code when future is successful */
    def whenFailed(code: PartialFunction[Throwable, Unit])(implicit ec: ExecutionContext): Future[A] = {
      future.onComplete {
        case Success(_) => // ignore
        case Failure(NonFatal(error)) =>
          if (code.isDefinedAt(error)) code.lift(error)
      }
      future
    }

    /** Remaps the failure with a given code */
    def mapFailure(code: PartialFunction[Throwable, Throwable])(implicit ec: ExecutionContext): Future[A] = {
      future.transform {
        case Failure(NonFatal(error)) if code.isDefinedAt(error) => Failure(code.apply(error))
        case result => result
      }
    }

    /** Writes the future result into transition when it's ready */
    def transition(transition: TransitionData[A])(implicit ec: ExecutionContext): Future[A] = {
      transition.loading
      future.whenSuccessful(v => transition.loaded(v))
      future.whenFailed {
        case TransitionException(code, reason) => transition.failed(code, reason)
        case other => transition.failed("unknown", other.getMessage)
      }
      future
    }
  }

  implicit class FutureListOps[A](val futures: List[Future[A]]) extends AnyVal {
    /** Executes futures one by one */
    def oneByOne(implicit ec: ExecutionContext): Future[List[A]] = futures match {
      case Nil => Future.successful(Nil)
      case head :: tail => for {
        value <- head
        others <- tail.oneByOne
      } yield value :: others
    }

    /** Executes futures in parallel */
    def flip(implicit ec: ExecutionContext): Future[List[A]] = Future.sequence(futures)
  }

  implicit class AnyListOps[A](val list: List[A]) extends AnyVal {
    /** Creates a processing chain from the list */
    def chainProcessing[B](last: Any, constructor: (A, Any) => B): Any = {
      list.foldRight(last) { case (element, next) => constructor.apply(element, next) }
    }

    /** Removes the value from the list */
    def without(value: A): List[A] = list.filterNot(a => a == value)

    /** Returns an element after the given seq value, if exists */
    def after(value: A): Option[A] = {
      list.indexOf(value) match {
        case -1 => None
        case last if last == list.size - 1 => None
        case index => Some(list(index + 1))
      }
    }

    /** Returns an element before the given seq value, if exists */
    def before(value: A): Option[A] = {
      list.indexOf(value) match {
        case -1 => None
        case 0 => None
        case index => Some(list(index - 1))
      }
    }
  }

  /** Integer 2D vector */
  case class Vec2i(x: Int, y: Int) {
    /** Adds components of another vector */
    def +(v: Vec2i): Vec2i = Vec2i(x + v.x, y + v.y)

    /** Adds components of another vector */
    def +(v: Vec2d): Vec2d = Vec2d(x + v.x, y + v.y)

    /** Subtracts components of another vector */
    def -(v: Vec2i): Vec2i = Vec2i(x - v.x, y - v.y)

    /** Multiplies components by given number */
    def *(m: Int): Vec2i = Vec2i(x * m, y * m)

    /** Multiplies components by given number */
    def *(d: Double): Vec2d = Vec2d(x * d, y * d)

    /** Multiplies components by given vector components */
    def *(v: Vec2d): Vec2d = Vec2d(x * v.x, y * v.y)

    /** Divides components by given vector components */
    def /(d: Double): Vec2d = Vec2d(x / d, y / d)

    /** Divides components by given vector components */
    def /(v: Vec2i): Vec2d = Vec2d(x / v.x.toDouble, y / v.y.toDouble)

    /** Converts vector into css style string */
    def style: String = s"width: ${x}px; height: ${y}px; "

    /** Converts vector into a list of indices */
    def range: List[Int] = (x to y).toList

    /** Negates the components */
    def flip: Vec2i = Vec2i(-x, -y)

    /** Negates the X */
    def flipX: Vec2i = Vec2i(-x, y)

    /** Negates the Y */
    def flipY: Vec2i = Vec2i(x, -y)

    /** Maps the X */
    def mapX(code: Int => Int): Vec2i = Vec2i(code.apply(x), y)

    /** Maps the Y */
    def mapY(code: Int => Int): Vec2i = Vec2i(x, code.apply(y))

    /** Fits the current dimensions into given bound scaling down or up */
    def fit(bounds: Vec2i): Vec2i = {
      val scales = bounds / this
      val scale = if (bounds.x < this.x || bounds.y < this.y) scales.min else scales.max
      (this * scale).toInt
    }

    /** Converts vector to doubles */
    def toDouble: Vec2d = Vec2d(x, y)

    /** Returns all points from 0:0 to the vector values */
    def iterate: List[(Int, Int)] = (0 until y).flatMap(yi => (0 until x).map(xi => (xi, yi))).toList
  }

  object Vec2i {
    /** Origin point */
    val Zero: Vec2i = 0 xy 0
  }

  /** Double 2D vector */
  case class Vec2d(x: Double, y: Double) {
    /** Adds components of another vector */
    def +(v: Vec2d): Vec2d = Vec2d(x + v.x, y + v.y)

    /** Subtracts components of another vector */
    def -(v: Vec2d): Vec2d = Vec2d(x - v.x, y - v.y)

    /** Multiplies components by given number */
    def *(m: Double): Vec2d = Vec2d(x * m, y * m)

    /** Multiplies components by given vector components */
    def *(v: Vec2d): Vec2d = Vec2d(x * v.x, y * v.y)

    /** Divides components by given number */
    def /(m: Double): Vec2d = Vec2d(x / m, y / m)

    /** Divides components by given vector components */
    def /(v: Vec2d): Vec2d = Vec2d(x / v.x, y / v.y)

    /** Returns minimum component */
    def min: Double = x min y

    /** Returns maximum component */
    def max: Double = x max y

    /** Returns the vector with maximum coordinates between the two */
    def maxVec(v: Vec2d): Vec2d = Vec2d(x max v.x, y max v.y)

    /** Returns true if given point is within the radius of the vector */
    def near(other: Vec2d, radius: Double): Boolean = {
      val diff = this - other
      val squared = diff * diff
      squared.x + squared.y < radius * radius
    }

    /** Offsets the X by X value of the given vector */
    def offsetX(v: Vec2d): Vec2d = Vec2d(x + v.x, y)

    /** Offsets the Y by Y value of the given vector */
    def offsetY(v: Vec2d): Vec2d = Vec2d(x, y + v.y)

    /** Converts vector to ints */
    def toInt: Vec2i = Vec2i(x.toInt, y.toInt)

    /** Adds XW vector to YH vector forming rectangle */
    def coordinateRect(yh: Vec2d): Rec2d = Rec2d(x xy yh.x, y xy yh.y)

    /** Returns vec with only x component */
    def onlyX: Vec2d = Vec2d(x, 0)

    /** Returns vec with only y component */
    def onlyY: Vec2d = Vec2d(x, 0)

    /** Returns a random vector within current values */
    def random(min: Vec2d = Vec2d.Zero, random: Random = new Random()): Vec2d = Vec2d(random.nextDouble() * (x - min.x) + min.x, random.nextDouble() * (y - min.y) + min.y)
  }

  object Vec2d {
    /** Left point in 1x1 square */
    val Left: Vec2d = 0.0 xy 0.5

    /** Center point in 1x1 square */
    val Center: Vec2d = 0.5 xy 0.5

    /** Right point in 1x1 square */
    val Right: Vec2d = 1.0 xy 0.5

    /** Top point in 1x1 square */
    val Top: Vec2d = 0.5 xy 0.0

    /** Bottom point in 1x1 square */
    val Bottom: Vec2d = 0.5 xy 1.0

    /** Top left point in 1x1 square */
    val TopLeft: Vec2d = 0.0 xy 0.0

    /** Top right point in 1x1 square */
    val TopRight: Vec2d = 1.0 xy 0.0

    /** Bottom left point in 1x1 square */
    val BottomLeft: Vec2d = 0.0 xy 1.0

    /** Bottom right point in 1x1 square */
    val BottomRight: Vec2d = 1.0 xy 1.0

    /** Origin point */
    val Zero: Vec2d = 0.0 xy 0.0
  }

  implicit class VecIntOps(val int: Int) extends AnyVal {
    /** Creates a vector from two numbers */
    def xy(y: Int): Vec2i = Vec2i(int, y)

    /** Creates a vector from two numbers */
    def xy(y: Double): Vec2d = Vec2d(int, y)
  }

  implicit class VecDoubleOps(val double: Double) extends AnyVal {
    /** Creates a vector from two numbers */
    def xy(y: Double): Vec2d = Vec2d(double, y)
  }

  implicit class VecDoubleTupleOps(val tuple: (Vec2d, Vec2d)) extends AnyVal {
    /** Returns a vec between two components according to progress */
    def %%(progress: Double): Vec2d = tuple._1 + (tuple._2 - tuple._1) * progress
  }

  /** Integer 2D rectangle */
  case class Rec2i(position: Vec2i, size: Vec2i) {
    /** Resizes the rectangle to given size */
    def resizeTo(size: Vec2i): Rec2i = copy(size = size)

    /** Moves the rectangle to a given position */
    def positionAt(position: Vec2i): Rec2i = copy(position = position)
  }

  object Rec2i {
    /** Zero size rectangle */
    val Zero: Rec2i = Rec2i(Vec2i.Zero, Vec2i.Zero)
  }

  /** Double 2D rectangle */
  case class Rec2d(position: Vec2d, size: Vec2d) {
    /** Resizes the rectangle to given size */
    def resizeTo(size: Vec2d): Rec2d = copy(size = size)

    /** Moves the rectangle to a given position */
    def positionAt(position: Vec2d): Rec2d = copy(position = position)

    /** Translates the position of the rectangle by given offset */
    def offsetBy(offset: Vec2d): Rec2d = copy(position = position + offset)

    /** Returns true if given point is within rectangle bounds */
    def contains(point: Vec2d): Boolean = {
      point.x >= position.x && point.y >= position.y && point.x <= position.x + size.x && point.y <= position.y + size.y
    }

    /** Returns true if the rectangle intersects with the given one, false if outside or touches the edges */
    def intersects(rec: Rec2d): Boolean = {
      val ouside =
        rec.position.x >= this.position.x + this.size.x ||
          this.position.x >= rec.position.x + rec.size.x ||
          rec.position.y >= this.position.y + this.size.y ||
          this.position.y >= rec.position.y + rec.size.y
      !ouside
    }

    /** Returns the rectangle area */
    def area: Double = size.x * size.y

    /** Returns the point within the rectangle at given scale */
    def pointAt(scale: Vec2d): Vec2d = position + size * scale
  }

  object Rec2d {
    /** Zero size rectangle */
    val Zero: Rec2d = Rec2d(Vec2d.Zero, Vec2d.Zero)

    /** Creates rectangle from the corner coordinates */
    def fromCorners(a: Vec2d, b: Vec2d): Rec2d = Rec2d(a, b - a)

    /** Creates rectangle that includes all of the given rectangles */
    def include(recs: Rec2d*): Rec2d = {
      val minX = recs.map(r => r.position.x).minOr(0.0)
      val maxX = recs.map(r => r.position.x + r.size.x).maxOr(0.0)
      val minY = recs.map(r => r.position.y).minOr(0.0)
      val maxY = recs.map(r => r.position.y + r.size.y).maxOr(0.0)
      fromCorners(minX xy minY, maxX xy maxY)
    }
  }

  implicit class OffsetDateTimeOps(val odt: OffsetDateTime) extends AnyVal {
    /** Converts date time to epoch */
    def epoch: Long = odt.toInstant.toEpochMilli

    /** Compares to given timestamp */
    def >(timestamp: Long): Boolean = odt.epoch > timestamp

    /** Compares to given timestamp */
    def <(timestamp: Long): Boolean = odt.epoch < timestamp

    /** Compares to given timestamp */
    def >=(timestamp: Long): Boolean = odt.epoch >= timestamp

    /** Compares to given timestamp */
    def <=(timestamp: Long): Boolean = odt.epoch <= timestamp
  }

  implicit class StringOps(val string: String) extends AnyVal {
    /** Parses string as duration: "1d2h3m4s5ms" */
    def duration: FiniteDuration = {
      val total = "([0-9]+)(d|h|ms|s|m)".r.findAllMatchIn(string).foldLeft(0L) { case (sum, part) =>
        val amount = part.group(1).toInt
        val duration = part.group(2) match {
          case "d" => amount.days
          case "h" => amount.hours
          case "m" => amount.minutes
          case "s" => amount.seconds
          case "ms" => amount.millis
        }
        sum + duration.toMillis
      }
      total.millis
    }
  }

  /** Contains color values in range [0-255] */
  case class Color(r: Double, g: Double, b: Double, a: Double = 255.0) {
    /** Converts the color to a single RGB integer */
    def toInt: Int = 65536 * r.toInt + 256 * g.toInt + b.toInt

    /** Converts the color to a single ARGB integer */
    def toArgb: Int = ((a.toInt & 0xFF) << 24) | ((r.toInt & 0xFF) << 16) | ((g.toInt & 0xFF) << 8) | ((b.toInt & 0xFF) << 0)

    /** Converts the color to a single RGB double */
    def toDouble: Double = toInt

    /** Converts the color to a single hex string */
    def toHex: String = f"#${r.toInt}%02x${g.toInt}%02x${b.toInt}%02x${a.toInt}%02x"

    /** Tins the color with given color by the given factor */
    def tint(other: Color, factor: Double): Color = Color(
      (r, other.r) %% factor,
      (g, other.g) %% factor,
      (b, other.b) %% factor,
      (a, other.a) %% factor
    )
  }

  object Colors {
    /** Parses color from hex string */
    def hex(string: String): Color = {
      val clear = string.replace("#", "")
      clear match {
        case hex if hex.length == 6 =>
          Color(
            r = Integer.valueOf(hex.substring(0, 2), 16).toDouble,
            g = Integer.valueOf(hex.substring(2, 4), 16).toDouble,
            b = Integer.valueOf(hex.substring(4, 6), 16).toDouble
          )
        case hex if hex.length == 8 =>
          Color(
            r = Integer.valueOf(hex.substring(0, 2), 16).toDouble,
            g = Integer.valueOf(hex.substring(2, 4), 16).toDouble,
            b = Integer.valueOf(hex.substring(4, 6), 16).toDouble,
            a = Integer.valueOf(hex.substring(6, 8), 16).toDouble
          )
      }
    }

    /** Parses color from ARGB int */
    def argb(int: Int): Color = {
      val r = (int >> 16) & 0xFF
      val g = (int >> 8) & 0xFF
      val b = (int >> 0) & 0xFF
      val a = (int >> 24) & 0xFF
      Color(r, g, b, a)
    }

    val PureBlack: Color = hex("#000000")
    val PureWhite: Color = hex("#ffffff")
    /** Material design colors */
    val Grey200: Color = hex("#eeeeee")
  }

  implicit class ColorOps(val color: Color) extends AnyVal {
    /** Returns the darker version of this color */
    def darker: Color = color match {
      case other => other.tint(Colors.PureBlack, 0.3)
    }

    /** Returns the lighter version of this color */
    def lighter: Color = color match {
      case other => other.tint(Colors.PureWhite, 0.3)
    }
  }

  /** Describes value that is eventually loaded */
  sealed trait Transition[A] {
    /** Returns Some(value) when successfully loaded */
    def valueOpt: Option[A]

    /** Returns true if the value is being loaded */
    def isTransitioning: Boolean = false

    /** Returns true if transition has failed to happen */
    def hasFailed: Boolean = false
  }

  /** Convenient type for writeable transitions */
  type TransitionData[A] = Writeable[Transition[A]]

  object Transition {

    /** Describes value that was not requested yet */
    case class Missing[A]() extends Transition[A] {
      override def valueOpt: Option[A] = None
    }

    /** Describes value that is being loaded right now */
    case class Loading[A](start: Long) extends Transition[A] {
      override def valueOpt: Option[A] = None

      override def isTransitioning: Boolean = true
    }

    /** Describes value that was successfully loaded */
    case class Loaded[A](start: Long, end: Long, value: A) extends Transition[A] {
      override def valueOpt: Option[A] = Some(value)
    }

    /** Describes value that failed to load */
    case class Failed[A](start: Long, end: Long, code: String, reason: String) extends Transition[A] {
      override def valueOpt: Option[A] = None

      override def hasFailed: Boolean = true
    }

    /** Exception that is expected for transition data */
    case class TransitionException(code: String, reason: String) extends RuntimeException

  }

  implicit class TransitionWritableOps[A](val transition: TransitionData[A]) extends AnyVal {
    /** Puts transition into loading state */
    def loading: TransitionData[A] = {
      transition() match {
        case Missing() =>
          transition.write(Loading(currentTimeMillis))
        case loading: Loading[A] =>
          transition.write(loading)
        case loaded: Loaded[A] =>
          transition.write(loaded)
        case failed: Failed[A] =>
          transition.write(Loading(currentTimeMillis))
      }
      transition
    }

    /** Puts transition into loaded state */
    def loaded(value: A): TransitionData[A] = {
      transition() match {
        case Missing() =>
          transition.write(Loaded(currentTimeMillis, currentTimeMillis, value))
        case Loading(start) =>
          transition.write(Loaded(start, currentTimeMillis, value))
        case loaded: Loaded[A] =>
          transition.write(loaded.copy(end = currentTimeMillis, value = value))
        case failed: Failed[A] =>
          transition.write(Loaded(currentTimeMillis, currentTimeMillis, value))
      }
      transition
    }

    /** Puts transition into failed state */
    def failed(code: String, reason: String): TransitionData[A] = {
      transition() match {
        case Missing() =>
          transition.write(Failed(currentTimeMillis, currentTimeMillis, code, reason))
        case Loading(start) =>
          transition.write(Failed(start, currentTimeMillis, code, reason))
        case loaded: Loaded[A] =>
          transition.write(Failed(currentTimeMillis, currentTimeMillis, code, reason))
        case failed: Failed[A] =>
          transition.write(Failed(currentTimeMillis, currentTimeMillis, code, reason))
      }
      transition
    }

    /** Resets the transition to missing state */
    def reset: TransitionData[A] = {
      transition.write(Missing())
      transition
    }

    /** Executes the code when transition is loading */
    def whenLoading(code: => Unit): TransitionData[A] = {
      transition /> {
        case Loading(start) => code
      }
      transition
    }

    /** Executes the code when transition was successfully loaded */
    def whenLoaded(code: A => Unit): TransitionData[A] = {
      transition /> {
        case Loaded(start, end, value) => code.apply(value)
      }
      transition
    }

    /** Executes the code when transition fails to load */
    def whenFailed(code: String => Unit): TransitionData[A] = {
      transition /> {
        case Failed(start, end, errorCode, reason) => code.apply(reason)
      }
      transition
    }

    /** Returns true for missing value */
    def isMissing: Boolean = transition() == Missing()

    /** Returns true for loaded value */
    def isLoaded: Boolean = transition().isInstanceOf[Loaded[A]]
  }

}