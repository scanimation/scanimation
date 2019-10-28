package scanimation

import scanimation.binary._
import scanimation.common._
import scanimation.format._

class BinarySpec extends Spec {
  val recompile = 1

  /** Marker trait for all messages */
  trait TestMessage

  case class Person(name: String, age: Int) extends TestMessage

  "binary" can {
    implicit val personFormat: BF[Person] = format2(Person)

    def check[A](a: A)(implicit format: BF[A]): Unit = {
      ByteList.empty
        .chain { bytes => format.append(Nil, a, bytes) }
        .chain { bytes => format.read(Nil, bytes) shouldBe(a, ByteList.empty) }
    }

    "format person" in {
      check(Person("John", 33))
    }

    "format two persons" in {
      val personA = Person("John", 42)
      val personB = Person("Amelie", 24)
      ByteList.empty
        .chain { bytes => personFormat.append(Nil, personA, bytes) }
        .chain { bytes => personFormat.append(Nil, personB, bytes) }
        .chain { bytes =>
          val (person, next) = personFormat.read(Nil, bytes)
          person shouldBe personA
          next
        }
        .chain { bytes =>
          val (person, next) = personFormat.read(Nil, bytes)
          person shouldBe personB
          next shouldBe ByteList.empty
        }
    }

    "format types" in {
      case class Types(string: String, int: Int, double: Double, boolean: Boolean)
      implicit val typesFormat: BF[Types] = format4(Types)

      check(Types("lorem", 42, 12.3, boolean = true))
      check(Types("", 0, -12.3, boolean = false))
    }

    "format lists" in {
      case class ListTypes(strings: List[String], ints: List[Int], doubles: List[Double], booleans: List[Boolean])
      implicit val listFormat: BF[ListTypes] = format4(ListTypes)

      check(ListTypes("lorem" :: "ipsum" :: Nil, 1 :: 2 :: 3 :: Nil, 1.2 :: 3.4 :: Nil, true :: false :: Nil))
    }

    "format options" in {
      case class OptionTypes(string: Option[String], int: Option[Int], double: Option[Double], boolean: Option[Boolean])
      implicit val optionFormat: BF[OptionTypes] = format4(OptionTypes)

      check(OptionTypes(Some("lorem"), Some(1), Some(1.2), Some(true)))
      check(OptionTypes(None, None, None, None))
    }

    "defined for person" in {
      personFormat.isDefinedFor(Person("John", 42)) shouldBe true
      personFormat.isDefinedFor("John") shouldBe false
    }

    "format huge payload" in {
      val people = (0 until 10000).map(index => Person(s"name$index", index)).toList
      val bytes = ByteList(people.toBinary.compact :: Nil)
      val actual = bytes.toScala[List[Person]]()
      actual shouldBe people
    }

    "format weird strings" in {
      check("That’s so freaking awesome!")
    }
  }

  "registry" can {
    case class Address(city: String, zip: Int) extends TestMessage

    val testRegistry = Registry[TestMessage](
      format2(Person),
      format2(Address)
    )

    def check[A <: TestMessage](a: A): Unit = {
      testRegistry.write(a).validate { bytes =>
        testRegistry.read(bytes) shouldBe(a, ByteList.empty)
      }
    }

    "format person" in {
      check(Person("John", 42))
    }

    "format address" in {
      check(Address("Jersey City", 1234))
    }
  }
}