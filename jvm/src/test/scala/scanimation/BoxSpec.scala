package scanimation

import scanimation.box._
import scanimation.common._
import scanimation.icon.MaterialDesign

//noinspection TypeAnnotation
class BoxSpec extends Spec {

  /** Returns 5 by 5 text dimensions for every symbol, with 1px spacing between them */
  trait MonoText extends BoxContext {
    override def measureText(text: String, font: Font, size: Double): Vec2d = text match {
      case "" => 0 xy 5
      case single if single.length == 1 => 5 xy 5
      case other => (other.length * 5 + (other.length - 1)) xy 5
    }
  }

  /** Represents a context without draw components */
  trait NotDrawable extends BoxContext {
    override def drawComponent: DrawComponent = ???

    override def registerCanvas(box: DrawingBox, canvas: Any): Unit = ???
  }

  /** Represents a context without root */
  trait NoRoot extends BoxContext {
    override def root: Box = ???
  }

  /** Represents a context that creates empty drawables */
  trait IgnoreDrawable extends BoxContext {
    override def drawComponent: DrawComponent = new DrawComponent {
      override def clear(): Unit = {}

      override def fill(area: Rec2d, color: Color, depth: Double): Unit = {}

      override def strokeRect(area: Rec2d, color: Color, width: Double): Unit = {}
    }

    override def registerCanvas(box: DrawingBox, canvas: Any): Unit = ???
  }

  /** Represents a context that ignores box registering */
  trait IgnoreRegister extends BoxContext {
    override def register(box: Box): Unit = {}

    override def unregister(box: Box): Unit = {}
  }

  trait SimpleContext {
    implicit val context: BoxContext = new MonoText with NotDrawable with NoRoot with IgnoreRegister
  }

  trait SimpleBase extends SimpleContext {
    implicit val styler: Styler = Styler.Empty
  }

  "box" can {
    "assign single hierarchy" in new SimpleBase {
      val a = container()
      val b = container()
      a.sub(b)
      a.layout.relChildren() shouldBe (b :: Nil)
      a.layout.absChildren() shouldBe (b :: Nil)
      b.layout.relParents() shouldBe (a :: Nil)
      b.layout.absParents() shouldBe (a :: Nil)

      a.sub()
      a.layout.relChildren() shouldBe Nil
      a.layout.absChildren() shouldBe Nil
      b.layout.relParents() shouldBe Nil
      b.layout.absParents() shouldBe Nil

      b.sub(a)
      b.layout.relChildren() shouldBe (a :: Nil)
      b.layout.absChildren() shouldBe (a :: Nil)
      a.layout.relParents() shouldBe (b :: Nil)
      a.layout.absParents() shouldBe (b :: Nil)
    }

    "assign 2-deep hierarchy" in new SimpleContext {
      implicit val styler: Styler = Styler.Empty
      val a = container()
      val b = container()
      val c = container()

      a.sub(
        b.sub(c)
      )
      a.layout.relChildren() shouldBe (b :: Nil)
      a.layout.absChildren() shouldBe (b :: c :: Nil)
      b.layout.relChildren() shouldBe (c :: Nil)
      b.layout.absChildren() shouldBe (c :: Nil)
      b.layout.relParents() shouldBe (a :: Nil)
      b.layout.absParents() shouldBe (a :: Nil)
      c.layout.relParents() shouldBe (b :: Nil)
      c.layout.absParents() shouldBe (b :: a :: Nil)

      a.sub()
      b.sub()
      a.layout.relChildren() shouldBe Nil
      a.layout.absChildren() shouldBe Nil
      b.layout.relChildren() shouldBe Nil
      b.layout.absChildren() shouldBe Nil
      b.layout.relParents() shouldBe Nil
      b.layout.absParents() shouldBe Nil
      c.layout.relParents() shouldBe Nil
      c.layout.absParents() shouldBe Nil

      c.sub(
        b.sub(a)
      )
      c.layout.relChildren() shouldBe (b :: Nil)
      c.layout.absChildren() shouldBe (b :: a :: Nil)
      b.layout.relChildren() shouldBe (a :: Nil)
      b.layout.absChildren() shouldBe (a :: Nil)
      b.layout.relParents() shouldBe (c :: Nil)
      b.layout.absParents() shouldBe (c :: Nil)
      a.layout.relParents() shouldBe (b :: Nil)
      a.layout.absParents() shouldBe (b :: c :: Nil)
    }

    "assign 2-wide hierarchy" in new SimpleBase {
      val a = container()
      val b = container()
      val c = container()

      a.sub(b, c)

      a.layout.relChildren() shouldBe (b :: c :: Nil)
      a.layout.absChildren() shouldBe (b :: c :: Nil)
      b.layout.relParents() shouldBe (a :: Nil)
      b.layout.absParents() shouldBe (a :: Nil)
      c.layout.relParents() shouldBe (a :: Nil)
      c.layout.absParents() shouldBe (a :: Nil)

      a.sub()
      a.layout.relChildren() shouldBe Nil
      a.layout.absChildren() shouldBe Nil
      b.layout.relParents() shouldBe Nil
      b.layout.absParents() shouldBe Nil
      c.layout.relParents() shouldBe Nil
      c.layout.absParents() shouldBe Nil
    }

    trait ContainerTree extends SimpleContext {
      val idA = BoxId("A")
      val idB = BoxId("B")
      val idC = BoxId("C")

      implicit val s: Styler = styler
      val boxC = container(id = idC)
      val boxB = container(id = idB).sub(boxC)
      val boxA = container(id = idA).sub(boxB)

      def styler: Styler = Styler.Empty
    }

    "layout empty containers" in new ContainerTree {
      override def styler: Styler = Styler.Empty

      boxA.layout.absBounds() shouldBe Rec2d.Zero
      boxB.layout.absBounds() shouldBe Rec2d.Zero
      boxC.layout.absBounds() shouldBe Rec2d.Zero
    }

    "pad containers" in new ContainerTree {
      override def styler: Styler = Styler.Empty

      boxC.pad(0.5 xy 1)
      boxB.pad(1 xy 2)
      boxA.pad(1.5 xy 3)

      boxA.layout.absBounds() shouldBe Rec2d(0 xy 0, 6 xy 12)
      boxB.layout.absBounds() shouldBe Rec2d(1.5 xy 3, 3 xy 6)
      boxC.layout.absBounds() shouldBe Rec2d(2.5 xy 5, 1 xy 2)
    }

    "pad containers with style" in new ContainerTree {
      val foo = isRegion && idA

      override def styler: Styler = StyleSheet(
        isContainer |> (_.pad(1 xy 1)),
        isContainer && idB |> (
          c => c.pad(2 xy 2)
          ),
        isContainer && idA |> (_.pad(3 xy 3))
      )

      boxA.layout.absBounds() shouldBe Rec2d(0 xy 0, 12 xy 12)
      boxB.layout.absBounds() shouldBe Rec2d(3 xy 3, 6 xy 6)
      boxC.layout.absBounds() shouldBe Rec2d(5 xy 5, 2 xy 2)
    }

    "fill and align containers" in new ContainerTree {
      override def styler: Styler = Styler.Empty

      boxA.layout.fixedW.write(Some(10))
      boxB.layout.fixedW.write(Some(4))
      boxB.layout.fixedH.write(Some(4))
      boxA.layout.absBounds() shouldBe Rec2d(0 xy 0, 10 xy 4)
      boxB.layout.absBounds() shouldBe Rec2d(3 xy 0, 4 xy 4)

      boxB.layout.align.write(Vec2d.Left)
      boxB.layout.absBounds() shouldBe Rec2d(0 xy 0, 4 xy 4)

      boxB.layout.align.write(Vec2d.Right)
      boxB.layout.absBounds() shouldBe Rec2d(6 xy 0, 4 xy 4)

      boxB.layout.fill.write(1 xy 1)
      boxB.layout.absBounds() shouldBe Rec2d(0 xy 0, 10 xy 4)
    }

    "handle id style" in new ContainerTree {
      override def styler: Styler = StyleSheet(
        isContainer && idB |> (_.pad(2 xy 2))
      )

      boxA.layout.absBounds() shouldBe Rec2d(0 xy 0, 4 xy 4)
      boxB.layout.absBounds() shouldBe Rec2d(0 xy 0, 4 xy 4)
      boxC.layout.absBounds() shouldBe Rec2d(2 xy 2, 0 xy 0)
    }

    "handle parent style" in new ContainerTree {
      override def styler: Styler = StyleSheet(
        isContainer && hasAbsParent(idA) |> (_.pad(1 xy 1))
      )

      boxA.layout.absBounds() shouldBe Rec2d(0 xy 0, 4 xy 4)
      boxB.layout.absBounds() shouldBe Rec2d(0 xy 0, 4 xy 4)
      boxC.layout.absBounds() shouldBe Rec2d(1 xy 1, 2 xy 2)
    }

    "handle and style" in new ContainerTree {
      override def styler: Styler = StyleSheet(
        isContainer && hasAbsParent(idA) && hasAbsChild(idC) |> (_.pad(1 xy 1))
      )

      boxA.layout.absBounds() shouldBe Rec2d(0 xy 0, 2 xy 2)
      boxB.layout.absBounds() shouldBe Rec2d(0 xy 0, 2 xy 2)
      boxC.layout.absBounds() shouldBe Rec2d(1 xy 1, 0 xy 0)
    }

    "layout text box" in new SimpleBase {
      val label = text()
      val button = container().sub(label).pad(10 xy 10)

      button.layout.absBounds() shouldBe Rec2d(0 xy 0, 20 xy 25)
      label.layout.absBounds() shouldBe Rec2d(10 xy 10, 0 xy 5)

      label.textValue("Hello, world!")
      button.layout.absBounds() shouldBe Rec2d(0 xy 0, 97 xy 25)
      label.layout.absBounds() shouldBe Rec2d(10 xy 10, 77 xy 5)
    }

    "layout button" in {
      implicit val context: BoxContext = new MonoText with IgnoreDrawable with NoRoot with IgnoreRegister
      implicit val styler: Styler = StyleSheet(
        isContainer |> (_.pad(10 xy 10))
      )
      val btn = textButton().text.textValue("Hello!")
      val root = container().sub(btn)

      root.layout.absBounds() shouldBe Rec2d(0 xy 0, 55 xy 25)
      btn.layout.absBounds() shouldBe Rec2d(10 xy 10, 35 xy 5)
    }

    "layout with screen size" in {
      implicit val context: BoxContext = new MonoText with IgnoreDrawable with NoRoot with IgnoreRegister
      val a = BoxId("a")
      val b = BoxId("b")
      val c = BoxId("c")
      implicit val styles: Styler = StyleSheet(
        isRegion && a |> (
          _.pad(20.0 xy 20.0),
          _.fillY,
        ),
        isRegion && b |> (_.pad(20.0 xy 20.0)),
        isText && c |> (_.textSize(20.0)),
      )
      val root = container(BoxId.Root)
      val boxA = region(a)
      val boxB = region(b)
      val textC = text(c).textValue("Hello, world!")
      root.sub(
        boxA.sub(
          boxB.sub(
            textC
          )
        )
      )
      root.layout.fixedW.write(Some(200))
      root.layout.fixedH.write(Some(200))

      root.layout.absBounds() shouldBe Rec2d(0 xy 0, 200 xy 200)
      root.layout.relBounds() shouldBe Rec2d(0 xy 0, 200 xy 200)
      textC.layout.relBounds() shouldBe Rec2d(20 xy 20, 77 xy 5)
    }

    "layout grid box" in new SimpleBase {
      //  +-----+--+---------+
      //  | aa  |ab| ac      | fill:  1, 0, 2
      //  |     |  |         | width: 20, 10, 20
      //  +-----+--+---------+
      //  | ba  |bb| bc      | fill:  1, 0, 2
      //  +-----+--+---------+ width: 10, 20, 30
      //  fill: 2,1
      //  height: 40, 20
      val aa = container(BoxId("aa")).fixedW(20).fillX.fixedH(40).fillY(2)
      val ab = container(BoxId("ab")).fixedW(10)
      val ac = container(BoxId("ac")).fixedW(20).fillX(2)
      val ba = container(BoxId("ba")).fixedW(10).fillX.fixedH(20).fillY
      val bb = container(BoxId("bb")).fixedW(20)
      val bc = container(BoxId("bc")).fixedW(30)

      grid()
        .mutate { g =>
          g.fixedW(140)
          g.fixedH(100)
          g.columns(3)
          g.pad(10.0 xy 5.0)
          g.spacing(5.0 xy 10.0)
        }
        .sub(
          aa, ab, ac,
          ba, bb, bc
        )

      aa.layout.absArea() shouldBe Rec2d(10 xy 5, 40 xy 40)
      ab.layout.absArea() shouldBe Rec2d(55 xy 5, 20 xy 40)
      ac.layout.absArea() shouldBe Rec2d(80 xy 5, 50 xy 40)

      ba.layout.absArea() shouldBe Rec2d(10 xy 55, 40 xy 40)
      bb.layout.absArea() shouldBe Rec2d(55 xy 55, 20 xy 40)
      bc.layout.absArea() shouldBe Rec2d(80 xy 55, 50 xy 40)
    }

    "layout hbox" in new SimpleBase {
      val a = container().fixedW(10).fillBoth.fixedH(10)
      val b = container().fixedW(20).fillY
      val c = container().fixedW(30).fillBoth.fixedH(20)
      hbox()
        .mutate { b =>
          b.fixedW(100)
          b.fixedH(50)
          b.pad(10 xy 10)
          b.spacingX(5)
        }
        .sub(a, b, c)

      a.layout.absArea() shouldBe Rec2d(10 xy 10, 20 xy 30)
      b.layout.absArea() shouldBe Rec2d(35 xy 10, 20 xy 30)
      c.layout.absArea() shouldBe Rec2d(60 xy 10, 30 xy 30)
    }

    "layout vbox" in new SimpleBase {
      val a = container().fixedH(10).fillBoth.fixedW(10)
      val b = container().fixedH(20).fillX
      val c = container().fixedH(30).fillBoth.fixedW(20)
      vbox()
        .mutate { b =>
          b.fixedW(50)
          b.fixedH(100)
          b.pad(10 xy 10)
          b.spacingY(5)
        }
        .sub(a, b, c)

      a.layout.absArea() shouldBe Rec2d(10 xy 10, 30 xy 20)
      b.layout.absArea() shouldBe Rec2d(10 xy 35, 30 xy 20)
      c.layout.absArea() shouldBe Rec2d(10 xy 60, 30 xy 30)
    }

    "layout free box" ignore new SimpleBase {
      val fill = region().fillBoth
      val a = container().fixedW(20).fixedH(10)
      val b = container().fixedW(5).fillY
      val c = container().fixedW(5).fixedH(5)
      fbox()
        .sub(a, b, c, fill)
        .assignPosition(b, 5 xy 5)
        .assignPosition(c, 20 xy 20)

      fill.layout.absArea() shouldBe Rec2d(0 xy 0, 25 xy 25)
      a.layout.absArea() shouldBe Rec2d(0 xy 0, 20 xy 10)
      b.layout.absArea() shouldBe Rec2d(5 xy 5, 5 xy 20)
      c.layout.absArea() shouldBe Rec2d(20 xy 20, 5 xy 5)
    }

    "stretch icon and text in hbox" in new SimpleBase {
      val iconBox = scanimation.box.icon.iconSize(10)
      val textBox = text().textValue("hi")
      val filler = container
      val sectionTitle = hbox.spacingX(5).fillX.fixedH(12).sub(
        iconBox,
        textBox,
        filler.fillBoth,
      )
      val sidebar = vbox.fixedW(50).sub(
        sectionTitle,
        container.fixedH(10)
      )
      val root = hbox.fixedW(100).sub(
        sidebar,
        container.fillBoth
      )

      root.layout.absBounds() shouldBe Rec2d(0 xy 0, 100 xy 22)
      sidebar.layout.absBounds() shouldBe Rec2d(0 xy 0, 50 xy 22)
      sectionTitle.layout.absBounds() shouldBe Rec2d(0 xy 0, 50 xy 12)
      iconBox.layout.absBounds() shouldBe Rec2d(0 xy 1, 10 xy 10)
      textBox.layout.absBounds() shouldBe Rec2d(15 xy 3.5, 11 xy 5)
      filler.layout.absBounds() shouldBe Rec2d(31 xy 0, 19 xy 12)
    }
  }
}