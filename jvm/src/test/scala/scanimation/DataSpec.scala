package scanimation

import java.util.concurrent.atomic.AtomicInteger

import scanimation.common._

class DataSpec extends Spec {
  implicit val listenerId: ListenerId = ListenerId()

  "data" can {
    "forget listeners" in {
      val counter = new AtomicInteger()
      val data = Data(0)

      data /> { case _ => counter.incrementAndGet() }
      counter.get() shouldBe 1

      data.write(1)
      counter.get() shouldBe 2

      data.forget()
      data.write(2)
      counter.get() shouldBe 2
    }

    "forget mapped listeners" in {
      val counter = new AtomicInteger()
      val data = Data(0)

      data.map(v => v * 2) /> { case _ => counter.incrementAndGet() }
      counter.get() shouldBe 1

      data.write(1)
      counter.get() shouldBe 2

      data.forget()
      data.write(2)
      counter.get() shouldBe 2
    }

    "forget projected listeners" in {
      val counter = new AtomicInteger()
      val data = Data(0)

      data /~ { case positive if positive > 0 => positive } /> { case _ => counter.incrementAndGet() }
      counter.get() shouldBe 1

      data.write(1)
      counter.get() shouldBe 2

      data.forget()
      data.write(2)
      counter.get() shouldBe 2
    }

    "forget and listeners" in {
      val counter = new AtomicInteger()
      val dataA = Data(0)
      val dataB = Data("")

      (dataA && dataB) /> { case _ => counter.incrementAndGet() }
      counter.get() shouldBe 1

      dataA.write(1)
      dataB.write("foo")
      counter.get() shouldBe 3

      dataA.forget()
      dataB.forget()
      dataA.write(2)
      dataB.write("bar")
      counter.get() shouldBe 3
    }

    "lazily trigger listeners" in {
      val counter = new AtomicInteger()
      val data = LazyData(0)

      data /> { case _ => counter.incrementAndGet() }
      counter.get() shouldBe 1
      data.write(0)
      counter.get() shouldBe 1

      data.write(1)
      counter.get() shouldBe 2
      data.write(1)
      counter.get() shouldBe 2
    }
  }
}