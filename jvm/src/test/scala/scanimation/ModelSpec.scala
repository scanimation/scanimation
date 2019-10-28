package scanimation

import scanimation.model._

class ModelSpec extends Spec {
  "model" can {
    "calculate closest note" in {
      calculateNote(440).label shouldBe "A4"
      calculateNote(880).label shouldBe "A5"
      calculateNote(220).label shouldBe "A3"

      calculateNote(466).label shouldBe "A#4"
      calculateNote(493).label shouldBe "B4"
      calculateNote(523).label shouldBe "C5"
      calculateNote(554).label shouldBe "C#5"

      calculateNote(261).label shouldBe "C4"
      calculateNote(247).label shouldBe "B3"

      calculateNote(2793).label shouldBe "F7"
      calculateNote(7902).label shouldBe "B8"
      calculateNote(16).label shouldBe "C0"
    }
  }
}