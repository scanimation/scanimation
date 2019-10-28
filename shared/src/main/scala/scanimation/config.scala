package scanimation

import scanimation.common._
import scanimation.format._

import scala.concurrent.duration._

object config {
  /** Global configuration reader */
  private var globalReader: ConfigReader = _

  /** Defines the global reader */
  def setGlobalReader(reader: ConfigReader): Unit = globalReader = reader

  /** Reads the config keys from runtime environment */
  trait ConfigReader {
    def get(path: Path): Option[String]
  }

  /** Reads the config keys from java runtime environment */
  object JvmReader extends ConfigReader {
    val envConfig: Map[String, String] = {
      sys.env.getOrElse("APP_CONFIG", "")
        .split('~').grouped(2).map(pair => pair.last)
        .filterNot(kv => kv == "")
        .map { kv =>
          val Array(key, value) = kv.split('=')
          key -> value
        }
        .toMap
    }

    override def get(path: Path): Option[String] = {
      val full = path.stringify
      sys.env.get(full).orElse(sys.props.get(full)).orElse(envConfig.get(full))
    }
  }

  type Config = List[(Path, String)]
  type ConfigFormat[A] = AbstractFormat[A, Config]
  type CF[A] = ConfigFormat[A]

  /** Configures the application using given default values and overrides from the reader */
  def configureNamespace[A](namespace: String, default: Option[A] = None)(implicit format: CF[A]): A = {
    val path = FieldPathSegment(namespace) :: Nil
    configurePath(path, default)
  }

  /** Configures the application using given default values and overrides from the reader */
  def configurePath[A](path: Path, default: Option[A] = None)(implicit format: CF[A]): A = {
    val defaultConfig = default.map(d => format.append(path, d, Nil)).getOrElse(Nil)
    format.read(path, defaultConfig)._1
  }

  implicit class ConfigOps(val formatted: Config) extends AnyVal {
    /** Converts the config to scala format */
    def toScala[A: CF](path: Path = Nil)(implicit format: CF[A]): A = format.read(path, formatted)._1
  }

  /** Provides the unit for config format */
  implicit val configType: FormatType[Config] = new FormatType[Config] {
    override def unit: Config = Nil
  }

  /** Reads strings */
  implicit val stringFormat: CF[String] = new CF[String] {
    override def read(path: Path, formatted: Config): (String, Config) = {
      val default = formatted.collectFirst { case (p, v) if p == path => v }
      val value = globalReader.get(path).orElse(default).getOrElse(error("missing string", path))
      value -> formatted
    }

    override def append(path: Path, a: String, formatted: Config): Config = {
      val value = path -> a
      formatted :+ value
    }
  }

  /** Reads booleans */
  implicit val booleanFormat: CF[Boolean] = stringFormat.map(v => v.toBoolean, v => v.toString)

  /** Reads ints */
  implicit val intFormat: CF[Int] = stringFormat.map(v => v.toInt, v => v.toString)

  /** Reads doubles */
  implicit val doubleFormat: CF[Double] = stringFormat.map(v => v.toDouble, v => v.toString)

  /** Reads finite durations */
  implicit val durationFormat: CF[FiniteDuration] = stringFormat.map(v => v.duration, v => v.prettyString)

  /** Reads lists of A */
  implicit def implicitListFormat[A: CF]: CF[List[A]] = new ConfigFormat[List[A]] {
    override def read(path: Path, formatted: Config): (List[A], Config) = {
      globalReader.get(path) match {
        case Some("[]") => Nil -> formatted
        case Some(other) => error(s"wrong list format: $other", path)
        case None =>
          val list = Stream
            .from(0)
            .takeWhile { i =>
              val fullPath = path :+ ArrayPathSegment(i)
              globalReader.get(fullPath).isDefined || formatted.exists { case (p, s) => p == fullPath }
            }
            .map(i => formatted.toScala[A](path :+ ArrayPathSegment(i)))
            .toList
          list -> formatted
      }
    }

    override def append(path: Path, a: List[A], formatted: Config): Config = {
      a.zipWithIndex.foldLeft(formatted) { case (current, (element, index)) =>
        current ++ element.format(path :+ ArrayPathSegment(index))
      }
    }
  }

  /** Reads optional A */
  implicit def implicitOptionFormat[A: CF]: CF[Option[A]] = implicitListFormat[A].map(list => list.headOption, option => option.toList)

  private def error(message: String, path: Path, cause: Option[Throwable] = None): Nothing = {
    throw new IllegalArgumentException(s"$message: ${path.stringify}", cause.orNull)
  }
}