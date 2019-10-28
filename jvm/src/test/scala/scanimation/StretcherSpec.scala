package scanimation

import scanimation.box._
import scanimation.common._

class StretcherSpec extends Spec {
  /** Stretches list of boxes */
  def stretch(list: List[Box], space: Double): List[Double] = {
    Stretcher.stretch[Box](list, b => b.layout.fill().x, b => b.layout.minW(), space).map { case (b, s) => s }
  }

  implicit val context: BoxContext = new BoxContext {
    override def drawComponent: DrawComponent = ???

    override def measureText(text: String, font: Font, size: Double): Vec2d = Vec2d.Zero

    override def register(box: Box): Unit = {}

    override def registerCanvas(box: DrawingBox, canvas: Any): Unit = {}

    override def root: Box = ???
  }

  implicit val styler: Styler = Styler.Empty

  "Stretcher" can {
    "stretch no fill boxes" in {
      stretch(List(
        container().fixedW(20),
        container().fixedW(20)
      ), 100) shouldBe List(20, 20)
    }

    "stretch two same fill 1 boxes" in {
      stretch(List(
        container().fillX.fixedW(20),
        container().fillX.fixedW(20)
      ), 100) shouldBe List(50, 50)
    }

    "stretch two different fill 1 boxes" in {
      stretch(List(
        container().fillX.fixedW(20),
        container().fillX.fixedW(40)
      ), 100) shouldBe List(50, 50)

      stretch(List(
        container().fillX.fixedW(40),
        container().fillX.fixedW(20)
      ), 100) shouldBe List(50, 50)

      stretch(List(
        container().fillX.fixedW(20),
        container().fillX.fixedW(60)
      ), 100) shouldBe List(40, 60)

      stretch(List(
        container().fillX.fixedW(60),
        container().fillX.fixedW(20)
      ), 100) shouldBe List(60, 40)
    }

    "stretch three different fill 1 boxes" in {
      stretch(List(
        container().fillX.fixedW(20),
        container().fillX.fixedW(30),
        container().fillX.fixedW(40)
      ), 100) shouldBe List(30, 30, 40)

      stretch(List(
        container().fillX.fixedW(40),
        container().fillX.fixedW(10),
        container().fillX.fixedW(10)
      ), 100) shouldBe List(40, 30, 30)
    }

    "stretch a mix of boxes" in {
      stretch(List(
        container().fillX.fixedW(20),
        container().fixedW(20),
        container().fillX.fixedW(20)
      ), 100) shouldBe List(40, 20, 40)

      stretch(List(
        container().fillX(2).fixedW(20),
        container().fillX.fixedW(20)
      ), 100) shouldBe List(60, 40)
    }
  }
}