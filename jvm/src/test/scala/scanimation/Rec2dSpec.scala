package scanimation

import scanimation.common._

class Rec2dSpec extends Spec {
  "Rec2d" can {
    "calculate intersections" in {
      Rec2d(0 xy 0, 10 xy 10) intersects Rec2d(0 xy 0, 0 xy 0) shouldBe false
      Rec2d(0 xy 0, 10 xy 10) intersects Rec2d(10 xy 0, 5 xy 5) shouldBe false
      Rec2d(0 xy 0, 10 xy 10) intersects Rec2d(0 xy 10, 5 xy 5) shouldBe false
      Rec2d(10 xy 0, 5 xy 5) intersects Rec2d(0 xy 0, 10 xy 10) shouldBe false
      Rec2d(0 xy 10, 5 xy 5) intersects Rec2d(0 xy 0, 10 xy 10) shouldBe false

      Rec2d(0 xy 0, 10 xy 10) intersects Rec2d(0 xy 5, 5 xy 5) shouldBe true
      Rec2d(0 xy 0, 10 xy 10) intersects Rec2d(0 xy 5, 5 xy 5) shouldBe true
      Rec2d(5 xy 0, 5 xy 5) intersects Rec2d(0 xy 0, 10 xy 10) shouldBe true
      Rec2d(5 xy 0, 5 xy 5) intersects Rec2d(0 xy 0, 10 xy 10) shouldBe true

      Rec2d(0 xy 0, 10 xy 10) intersects Rec2d(2 xy 2, 10 xy 1) shouldBe true
    }

    "create inclusion" in {
      Rec2d.include(Rec2d(0 xy 0, 5 xy 5), Rec2d(0 xy 0, 2 xy 2)) shouldBe Rec2d(0 xy 0, 5 xy 5)
      Rec2d.include(Rec2d(0 xy 0, 5 xy 5), Rec2d(10 xy 0, 2 xy 2)) shouldBe Rec2d(0 xy 0, 12 xy 5)
      Rec2d.include(Rec2d(0 xy 0, 5 xy 5), Rec2d(0 xy 10, 2 xy 2)) shouldBe Rec2d(0 xy 0, 5 xy 12)
      Rec2d.include(Rec2d(0 xy 0, 5 xy 5), Rec2d(10 xy 10, 2 xy 2)) shouldBe Rec2d(0 xy 0, 12 xy 12)
    }
  }
}