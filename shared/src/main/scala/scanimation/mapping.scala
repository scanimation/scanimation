package scanimation

import scanimation.common._
import scanimation.format._

import scala.concurrent.duration.FiniteDuration

/** Converts maps to case classes */
object mapping {
  type Mapping = Map[String, List[String]]
  type MappingFormat[A] = AbstractFormat[A, Mapping]
  type MF[A] = MappingFormat[A]

  /** Reads strings */
  implicit val stringFormat: MF[String] = new MF[String] {
    override def read(path: Path, formatted: Mapping): (String, Mapping) = {
      val value = formatted.get(path.stringify).toList.flatten.headOption.getOrElse(error("missing string", path))
      value -> formatted
    }

    override def append(path: Path, a: String, formatted: Mapping): Mapping = {
      val value = path.stringify -> (a :: Nil)
      formatted + value
    }
  }

  /** Reads ints */
  implicit val intFormat: MF[Int] = stringFormat.map(v => v.toInt, v => v.toString)

  /** Reads doubles */
  implicit val doubleFormat: MF[Double] = stringFormat.map(v => v.toDouble, v => v.toString)

  /** Reads finite durations */
  implicit val durationFormat: MF[FiniteDuration] = stringFormat.map(v => v.duration, v => v.prettyString)

  implicit class MappingStringOps(val value: String) extends AnyVal {
    /** Converts string value to scala */
    def toScala[A](implicit format: MF[A]): A = {
      val (a, f) = format.read(Nil, Map("" -> List(value)))
      a
    }
  }

  implicit class MappingOps(val map: Mapping) extends AnyVal {
    /** Converts map value to scala */
    def toScala[A](implicit format: MF[A]): A = {
      val (a, f) = format.read(Nil, map)
      a
    }
  }

  implicit class AnyMappingOps[A](val any: A) extends AnyVal {
    /** Converts scala value into a mapping */
    def toMapping(implicit format: MF[A]): Mapping = {
      format.append(Nil, any, Map.empty)
    }
  }

  /** Reads lists of A */
  implicit def implicitListFormat[A: MF]: MF[List[A]] = new MappingFormat[List[A]] {
    override def read(path: Path, formatted: Mapping): (List[A], Mapping) = {
      val list = formatted.getOrElse(path.stringify, Nil)
      list.map(string => string.toScala[A]) -> formatted
    }

    override def append(path: Path, as: List[A], formatted: Mapping): Mapping = {
      val strings = as.map(a => a.toMapping).flatMap(m => m.getOrElse("", Nil))
      formatted + (path.stringify -> strings)
    }
  }

  /** Reads optional A */
  implicit def implicitOptionFormat[A: MF]: MF[Option[A]] = implicitListFormat[A].map(list => list.headOption, option => option.toList)


  private def error(message: String, path: Path, cause: Option[Throwable] = None): Nothing = {
    throw new IllegalArgumentException(s"$message: ${path.stringify}", cause.orNull)
  }
}