import java.io.FileWriter

import scala.io.Source

val root = sys.env.getOrElse("PLACEHOLDER_ROOT", throw new IllegalStateException("PLACEHOLDER_ROOT env not found"))
val target = s"$root/macros/src/main/scala/cross/format.scala"
val count = 22
val (inputBefore, inputAfter, ignore) = Source.fromFile(target)
  .getLines().toList
  .foldLeft[(List[String], List[String], Int)](Nil, Nil, 0) {
  case ((before, after, phase), current) if !current.contains("GENERATED CODE") =>
    phase match {
      case 0 => (before :+ current, after, 0)
      case 1 => (before, after, 1)
      case 2 => (before, after :+ current, 2)
    }
  case ((before, after, phase), current) =>
    phase match {
      case 0 => (before :+ current, after, 1)
      case 1 => (before, after :+ current, 2)
    }
}
val formats = (0 to count).map { size =>
  val list = (0 until size).map(i => s"P${i + 1}")
  val types = (list :+ "A" :+ "B").mkString(",")
  val constructor = list.mkString(",")
  val implicits = if (list.isEmpty) "" else s"(implicit ${list.map(t => s"${t.toLowerCase}: AF[$t,B]").mkString(",")})"
  s"  def format$size[$types](constructor: ($constructor) => A)$implicits: AF[A,B] = macro macroFormat$size[A,B]"
}
val macroFormats = (0 to count).map { size =>
  val list = (0 until size).map(i => s"p${i + 1}")
  val params = if (list.isEmpty) "" else s"(${list.map(t => s"$t: c.Tree").mkString(",")})"
  val args = (list :+ "Nil").mkString("::")
  s"  def macroFormat$size[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)$params: c.Expr[AF[A,B]] = formatX(c)(constructor,$args)"
}
val operationFormats = (0 to count).map { size =>
  val list = (0 until size).map(i => "Nothing")
  val args = list.mkString(",")
  s"  def $$[A, B](constructor: ($args) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]"
}
val typedOperationFormat = (0 to count).map { size =>
  val list = (0 until size).map(i => "Nothing")
  val args = list.mkString(",")
  s"  def $$$$[A](constructor: ($args) => A): CaseClassType[A] = new CaseClassType[A] {}"
}
val output = inputBefore ++ formats ++ macroFormats ++ operationFormats ++ typedOperationFormat ++ inputAfter

output.foreach(println)

val writer = new FileWriter(target)
writer.write(output.mkString("\n"))
writer.close()