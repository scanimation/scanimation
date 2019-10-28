package scanimation

import scala.languageFeature.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.blackbox
import scala.util.control.NonFatal

object format {

  /** Reads and writes the object A as format B */
  trait AbstractFormat[A, B] {
    /** Reads the object A from formatted form B */
    def read(path: Path, formatted: B): (A, B)

    /** Writes the object A info formatted form B */
    def append(path: Path, a: A, formatted: B): B

    /** Tells whether or now the format supports the given value */
    def isDefinedFor(a: Any): Boolean = false

    /** Creates a new format based on this one and type mapping */
    def map[C](constructor: A => C, destructor: C => A): AbstractFormat[C, B] = {
      val delegate = this
      new AbstractFormat[C, B] {
        override def read(path: Path, formatted: B): (C, B) = {
          val (c, tail) = delegate.read(path, formatted)
          val mapped = try {
            constructor.apply(c)
          } catch {
            case NonFatal(up) => throw new IllegalArgumentException(s"failed to read: ${path.stringify}", up)
          }
          mapped -> tail
        }

        override def append(path: Path, c: C, formatted: B): B = {
          delegate.append(path, destructor.apply(c), formatted)
        }
      }
    }
  }

  type AF[A, B] = AbstractFormat[A, B]

  /** Defines the target type of formatting */
  trait FormatType[A] {
    def unit: A
  }

  /** Defines a segment of the path to the value */
  sealed trait PathSegment {
    /** Returns the string representation of this segment */
    def stringify: String
  }

  type Path = List[PathSegment]

  /** Defines the array element path */
  case class ArrayPathSegment(index: Int) extends PathSegment {
    override def stringify: String = index.toString
  }

  /** Defines the object field path */
  case class FieldPathSegment(name: String) extends PathSegment {
    override def stringify: String = name
  }

  implicit class PathListOps(val path: Path) extends AnyVal {
    /** Returns path in dot notation */
    def stringify: String = path.map(s => s.stringify).mkString(".")
  }

  implicit class ListFormatOps[A](val list: List[A]) extends AnyVal {
    /** Refers to any element of the list */
    def anyElement: A = ??? // replaced with macro

    /** Refers to some element of the list */
    def someElement: A = ??? // replaced with macro
  }

  implicit class AnyFormatOps[A](val a: A) extends AnyVal {
    /** Formats the object into B */
    def format[B](path: Path = Nil)(implicit fmt: AbstractFormat[A, B], tpe: FormatType[B]): B = fmt.append(path, a, tpe.unit)

    /** Appends the object to B */
    def format[B](path: Path, b: B)(implicit fmt: AbstractFormat[A, B]): B = fmt.append(path, a, b)

    /** Checks if values are equal */
    def $eq(value: A): Operation = ??? // replaced with macro

    /** Checks if values are not equal */
    def $neq(value: A): Operation = ??? // replaces with macro

    /** Checks if value is greater than given value */
    def $gt(value: A): Operation = ??? // replaced with macro

    /** Checks if value is greater than or equal to given value */
    def $gte(value: A): Operation = ??? // replaced with macro

    /** Checks if value is less than given value */
    def $lt(value: A): Operation = ??? // replaced with macro

    /** Checks if value is less than or equal to given value */
    def $lte(value: A): Operation = ??? // replaced with macro

    /** Checks if optional value exists or not */
    def $exists(value: Boolean): Operation = ??? // replace with macro

    /** Sorts the value in ascending order */
    def $asc: Operation = ??? // replaced with macro

    /** Sorts the value in descending order */
    def $desc: Operation = ??? // replaced with macro

    /** Checks if value matches one of the values */
    def $in(list: List[A]): Operation = ??? // replaced with macro
  }

  /** Operations allowed on paths */
  object Operations extends Enumeration {
    val EqualTo, NotEqualTo,
    GreaterThan, LessThan, GreaterThanOrEqualTo, LessThanOrEqualTo,
    In,
    IsNull, Exists,
    SortAsc, SortDesc = Value
  }

  /** Operation performed on path */
  sealed trait Operation

  /** Performs unary operation on path */
  case class UnaryOperation(path: Path, operation: Operations.Value) extends Operation

  /** Performs binary operation on path */
  case class BinaryOperation(path: Path, operation: Operations.Value, operand: Any) extends Operation

  trait OperationType[A] {
    /** Converts list of operations into desired format */
    def convert(operations: List[Operation]): A
  }

  trait CaseClassType[A] {
    /** Build the operations for case class A */
    def apply[B](operations: (A => Operation)*)(implicit converter: OperationType[B]): B = macro convertTypedOperations[B]
  }

  implicit val macros: macros = scala.language.experimental.macros

  /** Builds a formatter from type A to type B */
  def formatX[A: c.WeakTypeTag, B: c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree, parts: List[c.Tree]): c.Expr[AF[A, B]] = {
    import c.universe._
    val tpe = weakTypeOf[A]
    val fmt = weakTypeOf[B]
    val fields = tpe.decls.collectFirst { case primary: MethodSymbol if primary.isPrimaryConstructor => primary }.get.paramLists.head
    val read = {
      val (lines, names) = parts.zip(fields).zipWithIndex.map { case ((part, field), i) =>
        val value = TermName(s"value$i")
        val currentFormat = TermName(s"format$i")
        val nextFormat = TermName(s"format${i + 1}")
        val fieldName = field.name.decodedName.toString
        val line = (tail: c.Tree) => q"val ($value, $nextFormat) = $part.read(path :+ FieldPathSegment($fieldName), $currentFormat); $tail"
        line -> q"$value"
      }.unzip
      val lastBytes = TermName(s"format${parts.size}")
      val lastLine = (tail: c.Tree) => q"new $tpe(..$names) -> $lastBytes"
      mkblock(c)(lines :+ lastLine)
    }
    val append = {
      val lines = parts.zip(fields).zipWithIndex.map { case ((part, field), i) =>
        val currentFormat = TermName(s"format$i")
        val nextFormat = TermName(s"format${i + 1}")
        val accessor = TermName(field.fullName.split('.').last)
        val fieldName = field.name.decodedName.toString
        (tail: c.Tree) => q"val $nextFormat = $part.append(path :+ FieldPathSegment($fieldName), a.$accessor, $currentFormat); $tail"
      }
      val lastBytes = TermName(s"format${parts.size}")
      val lastLine = (tail: c.Tree) => q"$lastBytes"
      mkblock(c)(lines :+ lastLine)
    }
    c.Expr[AF[A, B]] {
      q"""
      new scanimation.format.AbstractFormat[$tpe, $fmt] {
        override def read(path: Path, format0: $fmt): ($tpe, $fmt) = { $read }
        override def append(path: Path, a: $tpe, format0: $fmt): $fmt = { $append }
        override def isDefinedFor(a: Any): Boolean = a match {
          case m: $tpe => true
          case other => false
        }
      }
      """
    }
  }


  /** Builds a handler A from list of operations */
  def convertOperations[A: c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(operations: c.Tree*)(converter: c.Tree): c.Expr[A] = {
    convertTypedOperations[A](c)(operations: _*)(converter)
  }

  /** Builds a handler A from list of operations */
  def convertTypedOperations[A: c.WeakTypeTag](c: blackbox.Context)(operations: c.Tree*)(converter: c.Tree): c.Expr[A] = {
    import c.universe._
    val elements = operations
      .map {
        case Function(_, q"""scanimation.format.AnyFormatOps[$tpe]($path) $$eq     $value""") =>
          (path, Some(value), q"""scanimation.format.Operations.EqualTo""")
        case Function(_, q"""scanimation.format.AnyFormatOps[$tpe]($path) $$neq    $value""") =>
          (path, Some(value), q"""scanimation.format.Operations.NotEqualTo""")
        case Function(_, q"""scanimation.format.AnyFormatOps[$tpe]($path) $$gt     $value""") =>
          (path, Some(value), q"""scanimation.format.Operations.GreaterThan""")
        case Function(_, q"""scanimation.format.AnyFormatOps[$tpe]($path) $$gte    $value""") =>
          (path, Some(value), q"""scanimation.format.Operations.GreaterThanOrEqualTo""")
        case Function(_, q"""scanimation.format.AnyFormatOps[$tpe]($path) $$lt     $value""") =>
          (path, Some(value), q"""scanimation.format.Operations.LessThan""")
        case Function(_, q"""scanimation.format.AnyFormatOps[$tpe]($path) $$lte    $value""") =>
          (path, Some(value), q"""scanimation.format.Operations.LessThanOrEqualTo""")
        case Function(_, q"""scanimation.format.AnyFormatOps[$tpe]($path) $$in     $value""") =>
          (path, Some(value), q"""scanimation.format.Operations.In""")
        case Function(_, q"""scanimation.format.AnyFormatOps[$tpe]($path) $$exists $value""") =>
          (path, Some(value), q"""scanimation.format.Operations.Exists""")
        case Function(_, q"""scanimation.format.AnyFormatOps[$tpe]($path) $$asc       """) =>
          (path, None, q"""scanimation.format.Operations.SortAsc""")
        case Function(_, q"""scanimation.format.AnyFormatOps[$tpe]($path) $$desc      """) =>
          (path, None, q"""scanimation.format.Operations.SortDesc""")
        case other =>
          c.abort(c.enclosingPosition, s"failed to recognize tree as expression: ${showCode(other)}")
      }
      .map {
        case (pathTree, valueOpt, operation) => (extractPath(c)(pathTree), valueOpt, operation)
      }
      .map {
        case (path, None, operation) =>
          q"""scanimation.format.UnaryOperation(List(..$path), $operation)"""
        case (path, Some(value), operation) =>
          q"""scanimation.format.BinaryOperation(List(..$path), $operation, $value)"""
      }
    c.Expr[A] {
      q"""
      $converter.convert(List(..$elements))
      """
    }
  }

  /** Builds a code block from given lines */
  private def mkblock(c: blackbox.Context)(lines: List[c.Tree => c.Tree]): c.Tree = {
    import c.universe._
    lines.foldRight(q"") { case (head, tail) =>
      q"""
      ${head.apply(tail)}
      """
    }
  }

  /** Extract the access path from the tree */
  private def extractPath(c: blackbox.Context)(tree: Trees#Tree): List[c.Tree] = {
    import c.universe._
    tree match {
      case q"""scanimation.format.ListFormatOps[$tpe]($head).anyElement""" => extractPath(c)(head) :+ q"""scanimation.format.ArrayPathSegment(-1)"""
      case q"""scanimation.format.ListFormatOps[$tpe]($head).someElement""" => extractPath(c)(head) :+ q"""scanimation.format.ArrayPathSegment(-2)"""
      case Select(head: c.Tree, TermName(tail)) => extractPath(c)(head) :+ q"""scanimation.format.FieldPathSegment($tail)"""
      case _ => Nil
    }
  }

  // @formatter:off
  // GENERATED CODE
  def format0[A,B](constructor: () => A): AF[A,B] = macro macroFormat0[A,B]
  def format1[P1,A,B](constructor: (P1) => A)(implicit p1: AF[P1,B]): AF[A,B] = macro macroFormat1[A,B]
  def format2[P1,P2,A,B](constructor: (P1,P2) => A)(implicit p1: AF[P1,B],p2: AF[P2,B]): AF[A,B] = macro macroFormat2[A,B]
  def format3[P1,P2,P3,A,B](constructor: (P1,P2,P3) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B]): AF[A,B] = macro macroFormat3[A,B]
  def format4[P1,P2,P3,P4,A,B](constructor: (P1,P2,P3,P4) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B]): AF[A,B] = macro macroFormat4[A,B]
  def format5[P1,P2,P3,P4,P5,A,B](constructor: (P1,P2,P3,P4,P5) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B]): AF[A,B] = macro macroFormat5[A,B]
  def format6[P1,P2,P3,P4,P5,P6,A,B](constructor: (P1,P2,P3,P4,P5,P6) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B]): AF[A,B] = macro macroFormat6[A,B]
  def format7[P1,P2,P3,P4,P5,P6,P7,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B]): AF[A,B] = macro macroFormat7[A,B]
  def format8[P1,P2,P3,P4,P5,P6,P7,P8,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B]): AF[A,B] = macro macroFormat8[A,B]
  def format9[P1,P2,P3,P4,P5,P6,P7,P8,P9,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8,P9) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B],p9: AF[P9,B]): AF[A,B] = macro macroFormat9[A,B]
  def format10[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B],p9: AF[P9,B],p10: AF[P10,B]): AF[A,B] = macro macroFormat10[A,B]
  def format11[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B],p9: AF[P9,B],p10: AF[P10,B],p11: AF[P11,B]): AF[A,B] = macro macroFormat11[A,B]
  def format12[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B],p9: AF[P9,B],p10: AF[P10,B],p11: AF[P11,B],p12: AF[P12,B]): AF[A,B] = macro macroFormat12[A,B]
  def format13[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B],p9: AF[P9,B],p10: AF[P10,B],p11: AF[P11,B],p12: AF[P12,B],p13: AF[P13,B]): AF[A,B] = macro macroFormat13[A,B]
  def format14[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B],p9: AF[P9,B],p10: AF[P10,B],p11: AF[P11,B],p12: AF[P12,B],p13: AF[P13,B],p14: AF[P14,B]): AF[A,B] = macro macroFormat14[A,B]
  def format15[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B],p9: AF[P9,B],p10: AF[P10,B],p11: AF[P11,B],p12: AF[P12,B],p13: AF[P13,B],p14: AF[P14,B],p15: AF[P15,B]): AF[A,B] = macro macroFormat15[A,B]
  def format16[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B],p9: AF[P9,B],p10: AF[P10,B],p11: AF[P11,B],p12: AF[P12,B],p13: AF[P13,B],p14: AF[P14,B],p15: AF[P15,B],p16: AF[P16,B]): AF[A,B] = macro macroFormat16[A,B]
  def format17[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B],p9: AF[P9,B],p10: AF[P10,B],p11: AF[P11,B],p12: AF[P12,B],p13: AF[P13,B],p14: AF[P14,B],p15: AF[P15,B],p16: AF[P16,B],p17: AF[P17,B]): AF[A,B] = macro macroFormat17[A,B]
  def format18[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B],p9: AF[P9,B],p10: AF[P10,B],p11: AF[P11,B],p12: AF[P12,B],p13: AF[P13,B],p14: AF[P14,B],p15: AF[P15,B],p16: AF[P16,B],p17: AF[P17,B],p18: AF[P18,B]): AF[A,B] = macro macroFormat18[A,B]
  def format19[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B],p9: AF[P9,B],p10: AF[P10,B],p11: AF[P11,B],p12: AF[P12,B],p13: AF[P13,B],p14: AF[P14,B],p15: AF[P15,B],p16: AF[P16,B],p17: AF[P17,B],p18: AF[P18,B],p19: AF[P19,B]): AF[A,B] = macro macroFormat19[A,B]
  def format20[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B],p9: AF[P9,B],p10: AF[P10,B],p11: AF[P11,B],p12: AF[P12,B],p13: AF[P13,B],p14: AF[P14,B],p15: AF[P15,B],p16: AF[P16,B],p17: AF[P17,B],p18: AF[P18,B],p19: AF[P19,B],p20: AF[P20,B]): AF[A,B] = macro macroFormat20[A,B]
  def format21[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B],p9: AF[P9,B],p10: AF[P10,B],p11: AF[P11,B],p12: AF[P12,B],p13: AF[P13,B],p14: AF[P14,B],p15: AF[P15,B],p16: AF[P16,B],p17: AF[P17,B],p18: AF[P18,B],p19: AF[P19,B],p20: AF[P20,B],p21: AF[P21,B]): AF[A,B] = macro macroFormat21[A,B]
  def format22[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22,A,B](constructor: (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22) => A)(implicit p1: AF[P1,B],p2: AF[P2,B],p3: AF[P3,B],p4: AF[P4,B],p5: AF[P5,B],p6: AF[P6,B],p7: AF[P7,B],p8: AF[P8,B],p9: AF[P9,B],p10: AF[P10,B],p11: AF[P11,B],p12: AF[P12,B],p13: AF[P13,B],p14: AF[P14,B],p15: AF[P15,B],p16: AF[P16,B],p17: AF[P17,B],p18: AF[P18,B],p19: AF[P19,B],p20: AF[P20,B],p21: AF[P21,B],p22: AF[P22,B]): AF[A,B] = macro macroFormat22[A,B]
  def macroFormat0[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,Nil)
  def macroFormat1[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::Nil)
  def macroFormat2[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::Nil)
  def macroFormat3[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::Nil)
  def macroFormat4[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::Nil)
  def macroFormat5[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::Nil)
  def macroFormat6[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::Nil)
  def macroFormat7[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::Nil)
  def macroFormat8[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::Nil)
  def macroFormat9[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree,p9: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::p9::Nil)
  def macroFormat10[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree,p9: c.Tree,p10: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::Nil)
  def macroFormat11[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree,p9: c.Tree,p10: c.Tree,p11: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::Nil)
  def macroFormat12[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree,p9: c.Tree,p10: c.Tree,p11: c.Tree,p12: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::Nil)
  def macroFormat13[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree,p9: c.Tree,p10: c.Tree,p11: c.Tree,p12: c.Tree,p13: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::Nil)
  def macroFormat14[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree,p9: c.Tree,p10: c.Tree,p11: c.Tree,p12: c.Tree,p13: c.Tree,p14: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::Nil)
  def macroFormat15[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree,p9: c.Tree,p10: c.Tree,p11: c.Tree,p12: c.Tree,p13: c.Tree,p14: c.Tree,p15: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::Nil)
  def macroFormat16[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree,p9: c.Tree,p10: c.Tree,p11: c.Tree,p12: c.Tree,p13: c.Tree,p14: c.Tree,p15: c.Tree,p16: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::Nil)
  def macroFormat17[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree,p9: c.Tree,p10: c.Tree,p11: c.Tree,p12: c.Tree,p13: c.Tree,p14: c.Tree,p15: c.Tree,p16: c.Tree,p17: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::Nil)
  def macroFormat18[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree,p9: c.Tree,p10: c.Tree,p11: c.Tree,p12: c.Tree,p13: c.Tree,p14: c.Tree,p15: c.Tree,p16: c.Tree,p17: c.Tree,p18: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::Nil)
  def macroFormat19[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree,p9: c.Tree,p10: c.Tree,p11: c.Tree,p12: c.Tree,p13: c.Tree,p14: c.Tree,p15: c.Tree,p16: c.Tree,p17: c.Tree,p18: c.Tree,p19: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::Nil)
  def macroFormat20[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree,p9: c.Tree,p10: c.Tree,p11: c.Tree,p12: c.Tree,p13: c.Tree,p14: c.Tree,p15: c.Tree,p16: c.Tree,p17: c.Tree,p18: c.Tree,p19: c.Tree,p20: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::Nil)
  def macroFormat21[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree,p9: c.Tree,p10: c.Tree,p11: c.Tree,p12: c.Tree,p13: c.Tree,p14: c.Tree,p15: c.Tree,p16: c.Tree,p17: c.Tree,p18: c.Tree,p19: c.Tree,p20: c.Tree,p21: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::Nil)
  def macroFormat22[A:c.WeakTypeTag,B:c.WeakTypeTag](c: blackbox.Context)(constructor: c.Tree)(p1: c.Tree,p2: c.Tree,p3: c.Tree,p4: c.Tree,p5: c.Tree,p6: c.Tree,p7: c.Tree,p8: c.Tree,p9: c.Tree,p10: c.Tree,p11: c.Tree,p12: c.Tree,p13: c.Tree,p14: c.Tree,p15: c.Tree,p16: c.Tree,p17: c.Tree,p18: c.Tree,p19: c.Tree,p20: c.Tree,p21: c.Tree,p22: c.Tree): c.Expr[AF[A,B]] = formatX(c)(constructor,p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::Nil)
  def $[A, B](constructor: () => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $[A, B](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => B)(operations: (B => Operation)*)(implicit converter: OperationType[A]): A = macro convertOperations[A]
  def $$[A](constructor: () => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  def $$[A](constructor: (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing) => A): CaseClassType[A] = new CaseClassType[A] {}
  // GENERATED CODE
  // @formatter:on 

}