package scanimation

import scanimation.format._
import scanimation.mapping._

class MappingSpec extends Spec {
  val recompile = 1

  case class Person(name: String, age: Int)

  implicit val personFormat: MF[Person] = format2(Person)

  "mapping" can {
    "format map into person" in {
      Map(
        "name" -> ("John" :: Nil),
        "age" -> ("42" :: Nil)
      ).toScala[Person] shouldBe Person("John", 42)
    }

    "format person into a map" in {
      Person("John", 42).toMapping shouldBe Map(
        "name" -> ("John" :: Nil),
        "age" -> ("42" :: Nil)
      )
    }

    "format options" in {
      case class Stuff(required: Int, option: Option[Int])
      implicit val format: MF[Stuff] = format2(Stuff)
      Stuff(42, Some(42)).toMapping shouldBe Map(
        "required" -> ("42" :: Nil),
        "option" -> ("42" :: Nil)
      )
      Stuff(42, None).toMapping shouldBe Map(
        "required" -> ("42" :: Nil),
        "option" -> Nil
      )
      Map(
        "required" -> ("42" :: Nil),
        "option" -> ("42" :: Nil)
      ).toScala[Stuff] shouldBe Stuff(42, Some(42))
      Map(
        "required" -> ("42" :: Nil),
        "option" -> Nil
      ).toScala[Stuff] shouldBe Stuff(42, None)
      Map(
        "required" -> ("42" :: Nil)
      ).toScala[Stuff] shouldBe Stuff(42, None)
    }

    "format lists" in {
      case class Stuff(required: Int, list: List[Int])
      implicit val format: MF[Stuff] = format2(Stuff)
      Stuff(42, List(42, 43)).toMapping shouldBe Map(
        "required" -> ("42" :: Nil),
        "list" -> ("42" :: "43" :: Nil)
      )
      Stuff(42, Nil).toMapping shouldBe Map(
        "required" -> ("42" :: Nil),
        "list" -> Nil
      )
      Map(
        "required" -> ("42" :: Nil),
        "list" -> ("42" :: Nil)
      ).toScala[Stuff] shouldBe Stuff(42, List(42))
      Map(
        "required" -> ("42" :: Nil),
        "list" -> Nil
      ).toScala[Stuff] shouldBe Stuff(42, Nil)
      Map(
        "required" -> ("42" :: Nil)
      ).toScala[Stuff] shouldBe Stuff(42, Nil)
    }
  }
}