package scanimation

import scanimation.box.IconStyle.IconValue
import scanimation.box.ImageStyle.ImageReference
import scanimation.common._
import scanimation.icon.MaterialDesign

import scala.reflect.ClassTag

//noinspection LanguageFeature
object box {

  /** Creates an instance of stack container */
  def container(id: BoxId = BoxId())(implicit context: BoxContext, assignedStyler: Styler): ContainerBox = {
    val assignedId = id
    new ContainerBox {
      override def id: BoxId = assignedId

      override def styler: Styler = assignedStyler
    }.bindAndRegister()
  }

  /** Creates an instance of stack container */
  def container(implicit context: BoxContext, assignedStyler: Styler): ContainerBox = this.container()

  /** Creates an instance of container with background color */
  def region(id: BoxId = BoxId())(implicit context: BoxContext, assignedStyler: Styler): RegionBox = {
    val assignedId = id
    new RegionBox {
      override val background: DrawComponent = context.drawComponent

      override def id: BoxId = assignedId

      override def styler: Styler = assignedStyler
    }.bindAndRegister()
  }

  /** Creates an instance of container with background color */
  def region(implicit context: BoxContext, assignedStyler: Styler): RegionBox = this.region()

  /** Creates an instance of box with text */
  def text(id: BoxId = BoxId())(implicit context: BoxContext, assignedStyler: Styler): TextBox = {
    val assignedId = id
    new TextBox {
      override def boxContext: BoxContext = context

      override def id: BoxId = assignedId

      override def styler: Styler = assignedStyler

      override def calculateLayoutX(): Unit = {}

      override def calculateLayoutY(): Unit = {}
    }.bindAndRegister()
  }

  /** Creates an instance of box with text */
  def text(implicit context: BoxContext, assignedStyler: Styler): TextBox = this.text()

  /** Creates an instance of button box with custom content */
  def button(id: BoxId = BoxId())(implicit context: BoxContext, assignedStyler: Styler): ContainerButtonBox = {
    val assignedId = id
    new ContainerButtonBox {
      override val background: DrawComponent = context.drawComponent

      override def id: BoxId = assignedId

      override def styler: Styler = assignedStyler
    }.bindAndRegister()
  }

  /** Creates an instance of button box with text label */
  def textButton(id: BoxId = BoxId())(implicit context: BoxContext, assignedStyler: Styler): TextButtonBox = {
    val assignedId = id
    val delegate = text()
    val button = new TextButtonBox {
      override val text: TextBox = delegate

      override val background: DrawComponent = context.drawComponent

      override def id: BoxId = assignedId

      override def styler: Styler = assignedStyler
    }.bindAndRegister()
    button.sub(delegate)
  }

  /** Creates an instance of button box with text label */
  def textButton(implicit context: BoxContext, assignedStyler: Styler): TextButtonBox = this.textButton()

  /** Creates an hbox button with icon on the left and text on the right */
  def iconTextButton(icon: IconValue, text: String, id: BoxId = BoxId())(implicit context: BoxContext, assignedStyler: Styler): ContainerButtonBox = {
    button(id).sub(
      hbox().sub(
        this.icon().mutate(_.iconValue(icon)),
        this.text().mutate(_.textValue(text))
      )
    )
  }

  /** Creates an hbox button with image on the left and text on the right */
  def imageTextButton(image: ImageReference, text: String, id: BoxId = BoxId())(implicit context: BoxContext, assignedStyler: Styler): ContainerButtonBox = {
    button(id).sub(
      hbox().sub(
        this.image().mutate(_.imageRef(image)),
        this.text().mutate(_.textValue(text))
      )
    )
  }

  /** Creates an instance of grid box container */
  def grid(id: BoxId = BoxId())(implicit context: BoxContext, assignedStyler: Styler): GridBox = {
    val assignedId = id
    new GridBox {
      override def id: BoxId = assignedId

      override def styler: Styler = assignedStyler
    }.bindAndRegister()
  }

  /** Creates an instance of horizontal box container */
  def hbox(id: BoxId = BoxId())(implicit context: BoxContext, assignedStyler: Styler): HBox = {
    val assignedId = id
    new HBox {
      override def id: BoxId = assignedId

      override def styler: Styler = assignedStyler
    }.bindAndRegister()
  }

  /** Creates an instance of horizontal box container */
  def hbox(implicit context: BoxContext, assignedStyler: Styler): HBox = this.hbox()

  /** Creates an instance of vertical box container */
  def vbox(id: BoxId = BoxId())(implicit context: BoxContext, assignedStyler: Styler): VBox = {
    val assignedId = id
    new VBox {
      override def id: BoxId = assignedId

      override def styler: Styler = assignedStyler
    }.bindAndRegister()
  }

  /** Creates an instance of vertical box container */
  def vbox(implicit context: BoxContext, assignedStyler: Styler): VBox = this.vbox()

  /** Creates an instance of icon box */
  def icon(id: BoxId = BoxId())(implicit context: BoxContext, assignedStyler: Styler): IconBox = {
    val assignedId = id
    new IconBox {
      override def id: BoxId = assignedId

      override def styler: Styler = assignedStyler
    }.bindAndRegister()
  }

  /** Creates an instance of icon box */
  def icon(implicit context: BoxContext, assignedStyler: Styler): IconBox = this.icon()

  /** Creates an instance of image box */
  def image(id: BoxId = BoxId())(implicit context: BoxContext, assignedStyler: Styler): ImageBox = {
    val assignedId = id
    new ImageBox {
      override def id: BoxId = assignedId

      override def styler: Styler = assignedStyler
    }.bindAndRegister()
  }

  /** Creates an instance of image box */
  def image(implicit context: BoxContext, assignedStyler: Styler): ImageBox = this.image()

  /** Creates an instance of free placement container box */
  def fbox(id: BoxId = BoxId())(implicit context: BoxContext, assignedStyler: Styler): FreeBox = {
    val assignedId = id
    new FreeBox {
      override def id: BoxId = assignedId

      override def styler: Styler = assignedStyler
    }.bindAndRegister()
  }

  /** Creates a box with drawing canvas */
  def drawBox(id: BoxId = BoxId())(implicit context: BoxContext, assignedStyler: Styler): DrawingBox = {
    val assignedId = id
    new DrawingBox {
      override def id: BoxId = assignedId

      override def styler: Styler = assignedStyler

      override def registerCanvas(canvas: Any): Unit = context.registerCanvas(this, canvas)
    }.bindAndRegister()
  }

  /** Selects any box */
  val anyBox: Selector[Box] = _ => true

  /** Selects boxes that implement given trait */
  def isA[A <: Box](implicit tag: ClassTag[A]): Selector[A] = box => tag.runtimeClass.isInstance(box)

  /** Selects boxes that has given id */
  def hasId(id: BoxId): Selector[Box] = box => box.id == id

  /** Selects boxes that has given class */
  def hasClass(clazz: BoxClass): Selector[Box] = box => box.classes.contains(clazz)

  /** Selects boxes with matching parent */
  def hasAbsParent(selector: Selector[_]): Selector[Box] = box => box.layout.absParents().exists(selector.appliesTo)

  /** Selects boxes with matching children */
  def hasAbsChild(selector: Selector[_]): Selector[Box] = box => box.layout.absChildren().exists(selector.appliesTo)

  /** Converts box ids to id selector */
  implicit def idSelector(id: BoxId): Selector[Box] = hasId(id)

  /** Converts box class to selector */
  implicit def classSelector(clazz: BoxClass): Selector[Box] = hasClass(clazz)

  /** Selects any box that is or within the box with given id */
  def under(selector: Selector[_]): Selector[Box] = {
    box => selector.appliesTo(box) || box.layout.absParents().exists(selector.appliesTo)
  }

  /** Selector for container boxes */
  val isContainer: Selector[ContainerBox] = isA[ContainerBox]

  /** Selector for region boxes */
  val isRegion: Selector[RegionBox] = isA[RegionBox]

  /** Selector for text boxes */
  val isText: Selector[TextBox] = isA[TextBox]

  /** Selector for text buttons */
  val isTextButton: Selector[TextButtonBox] = isA[TextButtonBox]

  /** Selector for custom buttons */
  val isButton: Selector[ContainerButtonBox] = isA[ContainerButtonBox]

  /** Selector for horizontal boxes */
  val isHBox: Selector[HBox] = isA[HBox]

  /** Selector for vertical boxes */
  val isVBox: Selector[VBox] = isA[VBox]

  /** Selector for icon boxes */
  val isIcon: Selector[IconBox] = isA[IconBox]

  /** Selector for image boxes */
  val isImage: Selector[ImageBox] = isA[ImageBox]

  /** The id of the box */
  case class BoxId(value: String = uuid) {
    override def toString: String = value
  }

  object BoxId {
    /** The id of the root box */
    val Root = BoxId("root")
  }

  /** Represents a 2D layout element */
  trait Box {
    /** Current layout of the box */
    private val boxLayout = Layout(self = this)
    boxLayout.bind()

    /** Returns the unique identifier of the element */
    def id: BoxId

    /** Returns the reference to styler which changes this box style */
    def styler: Styler

    /** Returns the current layout of the box */
    def layout: Layout = boxLayout

    /** Returns current style of the box */
    def style: Style = layout.style()

    /** Returns a list of classes that this box is assigned to */
    def classes: List[BoxClass] = layout.classes()

    /** Returns the whole hierarchy containing this element and everything below */
    def selfAndAbsoluteChildren: List[Box] = this :: layout.absChildren()

    /** Replaces current children with a given list of children */
    def sub(children: Box*): this.type = {
      val list = children.toList
      boxLayout.relChildren().foreach(child => child.updateParent(Nil))
      boxLayout.relChildren.write(list)
      list.foreach(child => child.updateParent(this :: Nil))
      this
    }

    /** Replaces current children with a given list of children */
    def subs(children: List[Box]): this.type = sub(children: _*)

    /** Refreshed the style of this box */
    def refreshStyle(): Unit = styler.apply(this)

    /** Updates the X layout of the box */
    def calculateLayoutX(): Unit

    /** Updates the Y layout of the box */
    def calculateLayoutY(): Unit

    /** Calculates the minimum width of the component */
    def calculateMinimumWidth: Double

    /** Calculates the minimum height of the component */
    def calculateMinimumHeight: Double

    /** Resets the parent to None */
    private def updateParent(parents: Boxes): Unit = {
      boxLayout.relParents.write(parents)
    }

    /** Updates the X area occupied by the box */
    def updateAreaX(x: Double, width: Double): Unit = {
      boxLayout.relAreaX.write(x xy width)
    }

    /** Updates the Y area occupied by the box */
    def updateAreaY(y: Double, height: Double): Unit = {
      boxLayout.relAreaY.write(y xy height)
    }

    /** Adds the class if enabled is true, removes otherwise */
    def updateClass(clazz: BoxClass, enabled: Boolean): this.type = {
      if (enabled) addClass(clazz) else removeClass(clazz)
      this
    }

    /** Adds the given class if not already present */
    def addClass(clazz: BoxClass): this.type = {
      val current = boxLayout.classes()
      if (!current.contains(clazz)) {
        boxLayout.classes.write(clazz :: current)
      }
      this
    }

    /** Removes the given class if present */
    def removeClass(clazz: BoxClass): this.type = {
      val current = boxLayout.classes()
      if (current.contains(clazz)) {
        boxLayout.classes.write(current.without(clazz))
      }
      this
    }

    /** Updates the fill.x to a given value */
    def fillX(fill: Double = 1.0): this.type = {
      boxLayout.fill.write(boxLayout.fill().copy(x = fill))
      this
    }

    /** Updates the fill.x to 1.0 */
    def fillX: this.type = this.fillX()

    /** Updates the fill.x to a given value */
    def fillY(fill: Double = 1.0): this.type = {
      boxLayout.fill.write(boxLayout.fill().copy(y = fill))
      this
    }

    /** Updates the fill.x to 1.0 */
    def fillY: this.type = this.fillY()

    /** Updates both fill.x and fill.y to the given value */
    def fillBoth(fill: Double = 1.0): this.type = {
      boxLayout.fill.write(fill xy fill)
      this
    }

    /** Updates both fill.x and fill.y to 1.0 */
    def fillBoth: this.type = this.fillBoth()

    /** Updates both fill.x and fill.y to 0 */
    def fillNone(): this.type = {
      boxLayout.fill.write(Vec2d.Zero)
      this
    }

    /** Sets the fixed width of the component */
    def fixedW(width: Double): this.type = {
      boxLayout.fixedW.write(Some(width))
      this
    }

    /** Sets the fixed height of the component */
    def fixedH(height: Double): this.type = {
      boxLayout.fixedH.write(Some(height))
      this
    }

    /** Sets the alignment of the box */
    def align(alignment: Vec2d): this.type = {
      boxLayout.align.write(alignment)
      this
    }

    /** Binds the box internal state */
    def bind(): Unit = {
      boxLayout.absEnabled /> { case flag =>
        updateClass(BoxClass.Enabled, flag)
        updateClass(BoxClass.Disabled, !flag)
      }
    }

    /** Binds the box styles and registers it within the context */
    def bindAndRegister()(implicit context: BoxContext): this.type = {
      this.bind()
      context.register(this)
      this
    }

    override def toString: String = s"Box($id)"
  }

  type Boxes = List[Box]
  type AnyStyleKey = StyleKey[_]

  /** Refers to the box */
  case class BoxRef[A <: Box](box: A)

  /** Represents a style of a 2D layout element */
  case class Style(parameters: Map[AnyStyleKey, Any]) {
    /** Sets the style parameter */
    def set[A](key: AnyStyleKey, value: A): Style = {
      copy(parameters = parameters + (key -> value))
    }

    /** Returns the value assign to style parameter */
    def get[A](key: AnyStyleKey): Option[A] = {
      parameters.get(key).map(a => a.asInstanceOf[A])
    }
  }

  /** Refers to a single parameter for a style */
  class StyleKey[A](box: Box, defaultValue: A, updatesSize: Boolean) {
    /** Writes a new style value */
    def apply(newValue: A): Box = {
      val lastValue = apply()
      if (newValue != lastValue) {
        box.layout.style.write(box.style.set(this, newValue))
        if (updatesSize) box.layout.styleSize.write(box.layout.styleSize() + 1)
      }
      box
    }

    /** Returns the currently set style value */
    def apply(): A = {
      box.layout.style().get[A](this).getOrElse(defaultValue)
    }

    override def toString: String = apply().toString
  }

  object StyleKey {
    /** Creates a style parameter reference */
    def apply[A](startingValue: A, box: Box): StyleKey[A] = new StyleKey(box, startingValue, true)
  }

  object VisualStyleKey {
    /** Creates a style parameter reference */
    def apply[A, B <: Box](startingValue: A, box: B, updatesSize: Boolean = true): StyleKey[A] = new StyleKey(box, startingValue, false)
  }

  /** Represents a box selector for style applications and searches */
  trait Selector[A <: Box] {
    /** Returns true if selector matches the box */
    def appliesTo(box: Box): Boolean

    /** Combines two selectors using AND */
    def &&[B >: A <: Box](other: Selector[B]): AndSelector[A] = this match {
      case AndSelector(delegates) => AndSelector((delegates :+ other).asInstanceOf[List[Selector[A]]])
      case s => AndSelector((s :: other :: Nil).asInstanceOf[List[Selector[A]]])
    }

    /** Combines two selectors using OR */
    def ||[B >: A <: Box](other: Selector[B]): OrSelector[A] = this match {
      case OrSelector(delegates) => OrSelector((delegates :+ other).asInstanceOf[List[Selector[A]]])
      case s => OrSelector((s :: other :: Nil).asInstanceOf[List[Selector[A]]])
    }

    /** Flips the selector scope */
    def not: Selector[A] = this match {
      case NotSelector(original) => original.asInstanceOf[Selector[A]]
      case s => NotSelector(s)
    }

    /** Converts selector into styler */
    def |>(modifiers: (A => Unit)*): Styler = Styler.apply(this) { case a => modifiers.foreach(m => m.apply(a)) }

    /** Converts selector into styler */
    def |>>(code: PartialFunction[A, Unit]): Styler = Styler.apply(this)(code)

    /** Groups sub styles together */
    def sub(stylers: Styler*): Styler = Styler.apply(this) { case a => stylers.foreach(s => s.lift.apply(a)) }
  }

  /** Combines a list of selectors as a grouped AND selector */
  case class AndSelector[A <: Box](delegates: List[Selector[_ <: A]]) extends Selector[A] {
    override def appliesTo(box: Box): Boolean = delegates.forall(s => s.appliesTo(box))
  }

  /** Combines a list of selectors as a grouped OR selector */
  case class OrSelector[A <: Box](delegates: List[Selector[_ <: A]]) extends Selector[A] {
    override def appliesTo(box: Box): Boolean = delegates.exists(s => s.appliesTo(box))
  }

  /** Flips the selector */
  case class NotSelector[A <: Box](delegate: Selector[_ <: A]) extends Selector[A] {
    override def appliesTo(box: Box): Boolean = !delegate.appliesTo(box)
  }

  /** Configures the style of the box */
  type Styler = PartialFunction[Box, Unit]

  object Styler {
    /** An empty styles that does not adjust box styles */
    val Empty: Styler = new Styler {
      override def isDefinedAt(x: Box): Boolean = false

      override def apply(x: Box): Unit = {}
    }

    /** Creates a styles based on list of selectors */
    def apply[A <: Box](selector: Selector[A])(code: PartialFunction[A, Unit]): Styler = new Styler {
      override def isDefinedAt(x: Box): Boolean = selector.appliesTo(x)

      override def apply(x: Box): Unit = code.lift.apply(x.asInstanceOf[A])
    }
  }

  object StyleSheet {
    /** Creates a compound styles from the list of small stylers */
    def apply(stylers: Styler*): Styler = new Styler {
      override def isDefinedAt(x: Box): Boolean = stylers.exists(s => s.isDefinedAt(x))

      override def apply(x: Box): Unit = stylers.foreach(s => s.lift.apply(x))
    }
  }

  /** Represents a group of similarly styled boxes */
  trait BoxClass {}

  object BoxClass {
    /** Interactive boxes that have mouse currently hovering over them */
    val Hover = BoxClass()
    /** Interactive boxes that were hovered when mouse was pressed */
    val Drag = BoxClass()
    /** Interactive boxes that are enabled for mouse interactions */
    val Enabled = BoxClass()
    /** Interactive boxes that are disabled for mouse interactions */
    val Disabled = BoxClass()

    /** Creates a unique box class */
    def apply(): BoxClass = new BoxClass() {}
  }

  /** Describes the current layout of the box
    *
    * @param self        reference to actual box
    * @param style       current visual style of the box
    * @param styleSize   a marker that is written every time style affecting component size is changed
    * @param classes     the current classes and states of the box
    * @param relChildren direct children of the box
    * @param absChildren all children below this box
    * @param relParents  a list of direct parents of the box
    * @param absParents  a list of all parents from farthest to closest
    * @param minW        minimum width of the box
    * @param minH        minimum height of the box
    * @param minSize     minimum size of the box
    * @param fixedW      optional fixed width of the box
    * @param fixedH      optional fixed height of the box
    * @param relAreaX    area X and width assigned to the box relative to parent
    * @param relAreaY    area Y and height assigned to the box relative to parent
    * @param relArea     area assigned to the box relative to parent
    * @param absAreaX    area X and width assigned to the box relative to root
    * @param absAreaY    area Y and height assigned to the box relative to root
    * @param absArea     area assigned to the box relative to root
    * @param relBoundsX  bounds X and width assigned to the box relative to parent
    * @param relBoundsY  bounds Y and height assigned to the box relative to parent
    * @param relBounds   bounds of the box relative to parent
    * @param absBoundsX  bounds X and width assigned to the box relative to root
    * @param absBoundsY  bounds Y and height assigned to the box relative to root
    * @param absBounds   bounds of the box relative to root
    * @param fill        how much free space the box should fill
    * @param align       alignment of the box bounds within assigned area
    * @param relVisible  whether the box itself is visible, invisible boxes still occupy screen space
    * @param absVisible  whether the box is visible and all parents are visible
    * @param relDisplay  whether the box itself is displayed, non-displayed boxes do not occupy screen space
    * @param absDisplay  whether the box is displayed and all parents are displayed
    * @param relEnabled  whether the box interactions are enabled
    * @param absEnabled  whether the box interactions are enabled and all parent interactions are enabled
    */
  case class Layout(self: Box,

                    style: Writeable[Style] = Data(Style(Map.empty)),
                    styleSize: Writeable[Long] = Data(1L),
                    classes: Writeable[List[BoxClass]] = LazyData(Nil),
                    relChildren: Writeable[Boxes] = LazyData(Nil),
                    absChildren: Writeable[Boxes] = LazyData(Nil),
                    relParents: Writeable[Boxes] = LazyData(Nil),
                    absParents: Writeable[Boxes] = LazyData(Nil),

                    minW: Writeable[Double] = LazyData(0),
                    minH: Writeable[Double] = LazyData(0),
                    minSize: Writeable[Vec2d] = LazyData(Vec2d.Zero),
                    fixedW: Writeable[Option[Double]] = LazyData(None),
                    fixedH: Writeable[Option[Double]] = LazyData(None),

                    relAreaX: Writeable[Vec2d] = LazyData(Vec2d.Zero),
                    relAreaY: Writeable[Vec2d] = LazyData(Vec2d.Zero),
                    relArea: Writeable[Rec2d] = LazyData(Rec2d.Zero),

                    absAreaX: Writeable[Vec2d] = LazyData(Vec2d.Zero),
                    absAreaY: Writeable[Vec2d] = LazyData(Vec2d.Zero),
                    absArea: Writeable[Rec2d] = LazyData(Rec2d.Zero),

                    relBoundsX: Writeable[Vec2d] = LazyData(Vec2d.Zero),
                    relBoundsY: Writeable[Vec2d] = LazyData(Vec2d.Zero),
                    relBounds: Writeable[Rec2d] = LazyData(Rec2d.Zero),

                    absBoundsX: Writeable[Vec2d] = LazyData(Vec2d.Zero),
                    absBoundsY: Writeable[Vec2d] = LazyData(Vec2d.Zero),
                    absBounds: Writeable[Rec2d] = LazyData(Rec2d.Zero),

                    fill: Writeable[Vec2d] = LazyData(Vec2d.Zero),
                    align: Writeable[Vec2d] = LazyData(Vec2d.Center),

                    relVisible: Writeable[Boolean] = LazyData(true),
                    absVisible: Writeable[Boolean] = LazyData(true),

                    relDisplay: Writeable[Boolean] = LazyData(true),
                    absDisplay: Writeable[Boolean] = LazyData(true),

                    relEnabled: Writeable[Boolean] = LazyData(true),
                    absEnabled: Writeable[Boolean] = LazyData(true)) {

    /** Unique subscription id for this layout */
    private val forgettableListenerId: ListenerId = ListenerId(self.id.value)

    /** List of parent data subscriptions */
    private val parentSubscriptions = List[Layout => Writeable[_]](
      _.absParents,
      _.absAreaX,
      _.absAreaY,
      _.absBoundsX,
      _.absBoundsY,
      _.absVisible,
      _.absDisplay,
      _.absEnabled
    )

    /** List of child data subscriptions */
    private val childSubscriptions = List[Layout => Writeable[_]](
      _.absChildren,
      _.minW,
      _.minH,
      _.relDisplay,
      _.fill
    )

    /** Recalculates and writes the minimum width of the box */
    def rewriteMinW(): Unit = minW.write(self.calculateMinimumWidth max fixedW().getOrElse(0.0))

    /** Recalculates and writes the minimum height of the box */
    def rewriteMinH(): Unit = minH.write(self.calculateMinimumHeight max fixedH().getOrElse(0.0))

    /** Binds all layout calculations */
    def bind(): Unit = {
      /** Calculates absolute parents and area */
      relParents />> { case (lastParents, nextParents) =>
        implicit val lid: ListenerId = forgettableListenerId
        lastParents.foreach { parent =>
          parentSubscriptions.foreach { code => code.apply(parent.layout).forget() }
        }
        minW.forget()
        minH.forget()
        relAreaX.forget()
        relAreaY.forget()
        relBoundsX.forget()
        relBoundsY.forget()
        relVisible.forget()
        relDisplay.forget()
        relEnabled.forget()

        nextParents.foreach { parent =>
          parent.layout.absParents /> { case grandparents => absParents.write(parent :: grandparents) }
          (parent.layout.absAreaX && relAreaX) /> { case (absParent, relSelf) => absAreaX.write(relSelf.offsetX(absParent)) }
          (parent.layout.absAreaY && relAreaY) /> { case (absParent, relSelf) => absAreaY.write(relSelf.offsetX(absParent)) }
          (parent.layout.absBoundsX && relBoundsX) /> { case (absParent, relSelf) => absBoundsX.write(relSelf.offsetX(absParent)) }
          (parent.layout.absBoundsY && relBoundsY) /> { case (absParent, relSelf) => absBoundsY.write(relSelf.offsetX(absParent)) }
          (parent.layout.absVisible && relVisible) /> { case (absParent, relSelf) => absVisible.write(absParent && relSelf) }
          (parent.layout.absDisplay && relDisplay) /> { case (absParent, relSelf) => absDisplay.write(absParent && relSelf) }
          (parent.layout.absEnabled && relEnabled) /> { case (abdParent, relSelf) => absEnabled.write(abdParent && relSelf) }
        }
        if (nextParents.isEmpty) {
          absParents.write(Nil)
          minW /> { case value => relAreaX.write(0 xy value) }
          minH /> { case value => relAreaY.write(0 xy value) }
          relAreaX /> { case value => absAreaX.write(value) }
          relAreaY /> { case value => absAreaY.write(value) }
          relBoundsX /> { case value => absBoundsX.write(value) }
          relBoundsY /> { case value => absBoundsY.write(value) }
          relVisible /> { case value => absVisible.write(value) }
          relDisplay /> { case value => absDisplay.write(value) }
          relEnabled /> { case value => absEnabled.write(value) }
        }
      }

      /** Calculates absolute children */
      relChildren />> { case (lastChildren, nextChildren) =>
        implicit val lid: ListenerId = forgettableListenerId
        lastChildren.foreach { child =>
          childSubscriptions.foreach { code => code.apply(child.layout).forget() }
        }
        nextChildren.foreach { child =>
          child.layout.absChildren /> { case _ =>
            absChildren.write(relChildren() ++ relChildren().flatMap(c => c.layout.absChildren()))
          }
          child.layout.minW /> { case _ =>
            rewriteMinW()
            self.calculateLayoutX()
          }
          child.layout.minH /> { case _ =>
            rewriteMinH()
            self.calculateLayoutY()
          }
          child.layout.relDisplay /> { case _ =>
            rewriteMinW()
            rewriteMinH()
            self.calculateLayoutX()
            self.calculateLayoutY()
          }
          child.layout.fill /> { case _ =>
            self.calculateLayoutX()
            self.calculateLayoutY()
          }
        }
        if (nextChildren.isEmpty) {
          absChildren.write(Nil)
          rewriteMinW()
          rewriteMinH()
        }
      }

      (minW && minH) /> { case (w, h) => minSize.write(w xy h) }
      (relAreaX && relAreaY) /> { case (x, y) => relArea.write(x coordinateRect y) }
      (absAreaX && absAreaY) /> { case (x, y) => absArea.write(x coordinateRect y) }
      (relBoundsX && relBoundsY) /> { case (x, y) => relBounds.write(x coordinateRect y) }
      (absBoundsX && absBoundsY) /> { case (x, y) => absBounds.write(x coordinateRect y) }

      /** Calculates bounds of the box */
      (fill.map(f => f.x) && align.map(a => a.x) && relAreaX && minW) /> { case (((fillX, alignX), Vec2d(areaX, areaW)), selfW) =>
        if (fillX > 0) relBoundsX.write(areaX xy areaW)
        else relBoundsX.write((areaX + alignX * (areaW - selfW)) xy selfW)
      }
      (fill.map(f => f.y) && align.map(a => a.y) && relAreaY && minH) /> { case (((fillY, alignY), Vec2d(areaY, areaH)), selfH) =>
        if (fillY > 0) relBoundsY.write(areaY xy areaH)
        else relBoundsY.write((areaY + alignY * (areaH - selfH)) xy selfH)
      }

      /** Calculates minimum size of the box */
      (classes && styleSize && fixedW) /> { case _ => rewriteMinW() }
      (classes && styleSize && fixedH) /> { case _ => rewriteMinH() }

      /** Updates the layout of the box children */
      (classes && styleSize && relBoundsX && relChildren) /> { case _ => self.calculateLayoutX() }
      (classes && styleSize && relBoundsY && relChildren) /> { case _ => self.calculateLayoutY() }

      /** Updates the style of the component */
      (classes && absParents && absChildren) /> { case _ => self.refreshStyle() }
    }
  }

  /** Delegates the creation of actual visual elements to implied implementation */
  trait BoxContext {
    /** Creates a new component with draw functionality */
    def drawComponent: DrawComponent

    /** Measures the space occupied by the text */
    def measureText(text: String, font: Font, size: Double): Vec2d

    /** Registers the box within the context */
    def register(box: Box): Unit

    /** Registers the drawing canvas on the page */
    def registerCanvas(box: DrawingBox, canvas: Any): Unit

    /** Returns the very root box that matches screen size */
    def root: Box
  }

  /** Context component that can draw within it's bounds */
  trait DrawComponent {
    /** Clears the draw component */
    def clear(): Unit

    /** Fills rectangle in the given area with given color */
    def fill(area: Rec2d, color: Color, depth: Double): Unit

    /** Outlines the rectangle in the given area */
    def strokeRect(area: Rec2d, color: Color, width: Double): Unit
  }

  /** Refers to interactive boxes */
  trait Interactive extends Box {
    /** True, if mouse is currently over the box */
    val hovering: Writeable[Boolean] = LazyData(false)
    /** True, while mouse is pressed and was first pressed over the element */
    val dragging: Writeable[Boolean] = LazyData(false)
    /** Updated when element is clicked */
    val click: Writeable[Unit] = Data()

    override def bind(): Unit = {
      super.bind()
      hovering /> { case flag => updateClass(BoxClass.Hover, flag) }
      dragging /> { case flag =>
        updateClass(BoxClass.Drag, flag)
        if (!flag && hovering() && layout.absEnabled()) click.write()
      }
    }

    /** Adds click event listener */
    def onClick(code: => Unit)(implicit listenerId: ListenerId = ListenerId()): this.type = {
      click.listen({ case _ => code }, false)
      this
    }
  }

  /** Represents a style for container boxes */
  trait ContainerStyle {
    this: Box =>
    /** Space between inner components and the edge of the box */
    lazy val pad = StyleKey(Vec2d.Zero, this)
    /** Offsets the children position by this amount */
    lazy val childOffset = StyleKey(Vec2d.Zero, this)
  }

  /** Container box with stackable children */
  trait ContainerBox extends Box with ContainerStyle {
    override def calculateLayoutX(): Unit = {
      layout.relChildren().foreach { child => child.updateAreaX(pad().x + childOffset().x, layout.relBounds().size.x - pad().x * 2) }
    }

    override def calculateLayoutY(): Unit = {
      layout.relChildren().foreach { child => child.updateAreaY(pad().y + childOffset().y, layout.relBounds().size.y - pad().y * 2) }
    }

    override def calculateMinimumWidth: Double = {
      val width = layout.relChildren().map(c => c.layout.minW()).maxOpt.getOrElse(0.0)
      width + pad().x * 2
    }

    override def calculateMinimumHeight: Double = {
      val height = layout.relChildren().map(c => c.layout.minH()).maxOpt.getOrElse(0.0)
      height + pad().y * 2
    }
  }

  /** Represents a style for regions */
  trait RegionStyle {
    this: Box =>
    /** Color used as a background of this region */
    lazy val fillColor = VisualStyleKey(Colors.PureBlack, this)
    /** Depth of the background fill of this region */
    lazy val fillDepth = VisualStyleKey(0.0, this)
    /** The width of the border around the region */
    lazy val borderWidth = VisualStyleKey(0.0, this)
    /** The color used to draw the border around the box */
    lazy val borderColor = VisualStyleKey(Colors.PureBlack, this)
  }

  /** Container box with background color */
  trait RegionBox extends ContainerBox with RegionStyle {
    val background: DrawComponent

    override def bind(): Unit = {
      super.bind()
      (layout.style && layout.relBounds) /> { case _ =>
        background.clear()
        background.fill(layout.relBounds(), fillColor(), fillDepth())
        if (borderWidth() > 0.0) background.strokeRect(layout.relBounds(), borderColor(), borderWidth())
      }
    }
  }

  /** Represents a style for components with text */
  trait TextStyle {
    this: Box =>
    /** The color of the box text */
    lazy val textColor = VisualStyleKey(Colors.PureBlack, this)
    /** The size of the box text in pixels */
    lazy val textSize = StyleKey(16.0, this)
    /** The font used to render box text */
    lazy val textFont = StyleKey(DefaultFont, this)
    /** The value of the text */
    lazy val textValue = StyleKey("", this)
  }

  /** Refers to a text font with cached metrics */
  case class Font(family: String) {
    /** Returns the size of the text string */
    def textMetric(text: String, size: Double)(implicit context: BoxContext): Vec2d = {
      context.measureText(text, this, size)
    }
  }

  /** Displays a line of text */
  trait TextBox extends Box with TextStyle {
    def boxContext: BoxContext

    /** Changes the text value of the box */
    def as(value: String): TextBox = this.mutate(_.textValue(value))

    override def calculateMinimumWidth: Double = {
      textFont().textMetric(textValue(), textSize())(boxContext).x
    }

    override def calculateMinimumHeight: Double = {
      textFont().textMetric("A", textSize())(boxContext).y
    }
  }

  /** Safe to use default font */
  val DefaultFont = Font("Arial")

  /** Represents a style for button components */
  trait ButtonStyle {
    this: Box =>
    /** The displayed cursor when hovered over the button */
    lazy val cursor = VisualStyleKey(Cursors.Auto, this)
  }

  /** Represents how mouse pointer looks */
  object Cursors extends Enumeration {
    val Auto, Pointer = Value
  }

  /** Interactive button box with text label */
  trait TextButtonBox extends ContainerButtonBox {
    val text: TextBox

    /** Sets the text value of the button */
    def as(value: String): TextButtonBox = this.mutate(_.text.as(value))
  }

  /** Interactive button box with custom content */
  trait ContainerButtonBox extends RegionBox with Interactive with RegionStyle with ButtonStyle {
  }

  /** Represents a style for container that puts children in a grid */
  trait GridStyle {
    this: Box =>
    /** The distance between grid cells */
    lazy val spacing = StyleKey(Vec2d.Zero, this)
    /** Number of columns in the grid */
    lazy val columns = StyleKey(1, this)
  }

  /** Container that arranges children in a grid */
  trait GridBox extends Box with GridStyle with ContainerStyle {
    /** Returns rows of relative children */
    def childrenRows: List[List[Box]] = {
      layout.relChildren().grouped(columns()).toList
    }

    /** Returns columns of relative children */
    def childrenColumns: List[List[Box]] = {
      val rows = childrenRows
      (0 until columns()).map(index => rows.flatMap(row => row.lift(index))).toList
    }

    override def calculateLayoutX(): Unit = {
      val columnBoxes = childrenColumns
      val sizes = Stretcher.stretch[List[Box]](
        list = columnBoxes,
        fillCode = col => col.map(b => b.layout.fill().x).maxOr(0.0),
        minCode = col => col.map(b => b.layout.minW()).maxOr(0.0),
        layout.relAreaX().y - ((columnBoxes.size - 1) max 0) * spacing().x - pad().x * 2
      )
      sizes.foldLeft(pad().x) { case (offset, (column, size)) =>
        column.foreach(c => c.updateAreaX(offset + childOffset().x, size))
        offset + size + spacing().x
      }
    }

    override def calculateLayoutY(): Unit = {
      val rowBoxes = childrenRows
      val sizes = Stretcher.stretch[List[Box]](
        list = rowBoxes,
        fillCode = row => row.map(b => b.layout.fill().y).maxOr(0.0),
        minCode = row => row.map(b => b.layout.minH()).maxOr(0.0),
        layout.relAreaY().y - ((rowBoxes.size - 1) max 0) * spacing().y - pad().y * 2
      )
      sizes.foldLeft(pad().y) { case (offset, (row, size)) =>
        row.foreach(c => c.updateAreaY(offset + childOffset().y, size))
        offset + size + spacing().y
      }
    }

    override def calculateMinimumWidth: Double = {
      val cols = childrenColumns
      val width = cols
        .map(col => col.map(c => c.layout.minW()).maxOr(0.0))
        .sum
      width + pad().x * 2 + (cols.size - 1) * spacing().x
    }

    override def calculateMinimumHeight: Double = {
      val rows = childrenRows
      val height = rows
        .map(row => row.map(c => c.layout.minH()).maxOr(0.0))
        .sum
      height + pad().y * 2 + (rows.size - 1) * spacing().y
    }
  }

  /** Box that places a list of components horizontally */
  trait HBox extends GridBox {
    override def bind(): Unit = {
      super.bind()
      layout.relChildren /> { case children => columns(children.size max 1) }
    }

    /** Sets the horizontal spacing between children */
    def spacingX(value: Double): this.type = {
      spacing(value xy spacing().y)
      this
    }
  }

  /** Box that places a list of components vertically */
  trait VBox extends GridBox {
    override def bind(): Unit = {
      super.bind()
      columns(1)
    }

    /** Sets the vertical spacing between children */
    def spacingY(value: Double): this.type = {
      spacing(spacing().x xy value)
      this
    }
  }

  /** Style for boxes with icons */
  trait IconStyle {
    this: Box =>
    /** The actual image of the icon */
    lazy val iconValue = VisualStyleKey(MaterialDesign.`3dRotation`, this)
    /** The icon fill color */
    lazy val iconColor = VisualStyleKey(Colors.PureBlack, this)
    /** The width and height of the icon */
    lazy val iconSize = StyleKey(16.0, this)
  }

  object IconStyle {

    /** Actual icon value with native representation */
    case class IconValue(family: IconFamily, native: String)

    /** A set of icons */
    trait IconFamily

  }

  /** Box that renders a scalable icon */
  trait IconBox extends Box with IconStyle {
    /** Updates the icon value */
    def as(icon: IconValue): IconBox = this.mutate(_.iconValue(icon))

    override def calculateMinimumWidth: Double = iconSize()

    override def calculateMinimumHeight: Double = iconSize()

    override def calculateLayoutX(): Unit = {}

    override def calculateLayoutY(): Unit = {}
  }

  implicit class IconValueOps(val value: IconValue) extends AnyVal {
    /** Builds a box out of the icon */
    def box(id: BoxId = BoxId())(implicit boxContext: BoxContext, styler: Styler): IconBox = icon(id).mutate(_.iconValue(value))
  }

  /** Style for boxes with images */
  trait ImageStyle {
    this: Box =>
    /** Contains a reference to an image compiled into a tileset */
    lazy val imageRef = StyleKey(ImageStyle.EmptyImage, this)
  }

  object ImageStyle {

    /** Contains multiple images in a single file */
    case class Tileset(path: String) {
      var images: List[ImageReference] = Nil

      /** Returns the reference to image source */
      def source(path: String): ImageSource = ImageSource(path, this)

      /** Appends image to tileset registry */
      def register(ref: ImageReference): Unit = images = images :+ ref

      /** Path to tileset data */
      def dataPath: String = s"$path.ts"

      /** Path to tileset image */
      def imagePath: String = s"$path.png"
    }

    /** Refers to original image */
    case class ImageSource(path: String, tileset: Tileset) {
      /** Returns a reference to the image source with modified properties */
      def ref(size: Vec2d = Vec2d.Zero, color: Color => Color = c => c): ImageReference = {
        val reference = ImageReference(this, size, color)
        tileset.register(reference)
        reference
      }
    }

    /** Refers to a copy of original image */
    case class ImageReference(source: ImageSource, size: Vec2d, color: Color => Color)

    /** Refers to an image source */
    case class ImageValue(source: ImageReference, area: Rec2d)

    /** Tileset that contains single empty image */
    val EmptyTileset: Tileset = Tileset("/tileset/empty")
    /** Reference to empty transparent image */
    val EmptyImage: ImageReference = EmptyTileset.source("/image/empty.png").ref()
  }

  implicit class ImageRefOps(val value: ImageReference) extends AnyVal {
    /** Builds a box out of the image reference */
    def box(id: BoxId = BoxId())(implicit boxContext: BoxContext, styler: Styler): ImageBox = image(id).mutate(_.imageRef(value))
  }

  /** Style for container boxes with free positions */
  trait FreeStyle {
    this: Box =>
    /** Contains the position of child boxes */
    lazy val positions = StyleKey(Map.empty[BoxId, Vec2d], this)

    /** Sets the position for a given box to a given value */
    def assignPosition(box: Box, position: Vec2d): this.type = assignPositionId(box.id, position)

    /** Sets the position for a box with given id to a given value */
    def assignPositionId(id: BoxId, position: Vec2d): this.type = {
      positions(positions() + (id -> position))
      this
    }
  }

  /** Box that renders image of o static size */
  trait ImageBox extends Box with ImageStyle {
    /** Updates the image value */
    def as(image: ImageReference): ImageBox = this.mutate(_.imageRef(image))

    override def calculateMinimumWidth: Double = imageRef().size.x

    override def calculateMinimumHeight: Double = imageRef().size.y

    override def calculateLayoutX(): Unit = {}

    override def calculateLayoutY(): Unit = {}
  }

  /** Container with children positioned at freely assigned locations */
  trait FreeBox extends Box with FreeStyle {
    override def calculateMinimumWidth: Double = {
      layout.relChildren()
        .map(c => positions().getOrElse(c.id, Vec2d.Zero).x + c.layout.minW())
        .maxOr(0.0)
    }

    override def calculateMinimumHeight: Double = {
      layout.relChildren()
        .map(c => positions().getOrElse(c.id, Vec2d.Zero).y + c.layout.minH())
        .maxOr(0.0)
    }

    override def calculateLayoutX(): Unit = {
      layout.relChildren().foreach { child =>
        val pos = positions().getOrElse(child.id, Vec2d.Zero).x
        child.updateAreaX(pos, layout.relBounds().size.x - pos)
      }
    }

    override def calculateLayoutY(): Unit = {
      layout.relChildren().foreach { child =>
        val pos = positions().getOrElse(child.id, Vec2d.Zero).y
        child.updateAreaY(pos, layout.relBounds().size.y - pos)
      }
    }
  }

  /** Represents a box with canvas inside */
  trait DrawingBox extends ContainerBox {
    /** Registers the drawing canvas on the page once it's ready */
    def registerCanvas(canvas: Any): Unit
  }

  object Stretcher {
    /** Distributes the free space between stretchable list of objects */
    def stretch[A <: AnyRef](list: List[A], fillCode: A => Double, minCode: A => Double, totalSpace: Double): List[(A, Double)] = {
      val minCache = list.map(a => a -> minCode.apply(a)).toMap
      val array = list.flatMap { a =>
        val fill = fillCode.apply(a)
        val min = minCache(a)
        if (fill > 0) Some(Stretchable(a, min, fill, min)) else None
      }.sortBy(s => s.size).toArray

      var space = totalSpace - minCache.values.sum

      // stretch up to the largest size
      var index = 0
      var leave = false
      while (index < array.length && !leave) {
        val current = array(index)
        val currentSize = current.size
        val nextSizeOpt = array.lift(index + 1).map(s => s.size)
        nextSizeOpt match {
          case None =>
            // proceed to stretch the whole array
            leave = true
          case Some(same) if same == currentSize => // do nothing, skip to the next member
          case Some(nextSize) =>
            // stretch all previous members up to the nextSize
            val sizePerMember = (space / (index + 1)) min (nextSize - currentSize)
            space = space - sizePerMember * (index + 1)
            array.take(index + 1).foreach(s => s.size = s.size + sizePerMember)
        }
        index = index + 1
      }

      // stretch the leftover size
      val totalFill = array.map(s => s.fill).sum
      if (totalFill > 0) {
        val sizePerFill = space / totalFill
        array.foreach(s => s.size = s.size + s.fill * sizePerFill)
      }

      // restore order and return
      list.map { a =>
        a -> array
          .collectFirst { case s if s.value.eq(a) => s.size }
          .getOrElse(minCache(a))
      }
    }

    /** Stores the stretching data and results
      *
      * @param value the stretched object
      * @param size  the resulting size of stretched object
      * @param fill  how much space should the object take from free space
      * @param min   the minimum size of an object
      * @tparam A the object type
      */
    private case class Stretchable[A <: AnyRef](value: A, var size: Double, fill: Double, min: Double)

  }

}