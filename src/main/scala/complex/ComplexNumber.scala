package complex

import scala.language.implicitConversions
import scala.util.matching.Regex

// DO NOT CHANGE ANYTHING BELOW
final case class ComplexNumber(real: Double, imaginary: Double) {
  def *(other: ComplexNumber) =
    ComplexNumber(
      (real * other.real) - (imaginary * other.imaginary),
      (real * other.imaginary) + (imaginary * other.real)
    )
  def +(other: ComplexNumber) =
    ComplexNumber(real + other.real, imaginary + other.imaginary)
  def ~=(o: ComplexNumber) =
    (real - o.real).abs < 1e-6 && (imaginary - o.imaginary).abs < 1e-6
}

object ComplexNumber {
  // DO NOT CHANGE ANYTHING ABOVE
  implicit class ComplexNumberOps(val c: ComplexNumber) extends AnyVal {
    def -(other: ComplexNumber): ComplexNumber =
      ComplexNumber(c.real - other.real, c.imaginary - other.imaginary)

    def /(other: ComplexNumber): ComplexNumber = {
      val denominator = other.real * other.real + other.imaginary * other.imaginary
      if (denominator == 0) throw new ArithmeticException("Division by zero")
      ComplexNumber(
        (c.real * other.real + c.imaginary * other.imaginary) / denominator,
        (c.imaginary * other.real - c.real * other.imaginary) / denominator
      )
    }

    def toPolar: (Double, Double) = {
      val r = math.sqrt(c.real * c.real + c.imaginary * c.imaginary)
      val theta = math.atan2(c.imaginary, c.real)
      (r, theta)
    }
  }

  implicit def doubleToComplexNumber(d: Double): ComplexNumber = ComplexNumber(d, 0)

  implicit object ComplexNumeric extends Numeric[ComplexNumber] {
    def plus(x: ComplexNumber, y: ComplexNumber): ComplexNumber = x + y

    def minus(x: ComplexNumber, y: ComplexNumber): ComplexNumber = x + y

    def times(x: ComplexNumber, y: ComplexNumber): ComplexNumber = x * y

    def negate(x: ComplexNumber): ComplexNumber = ComplexNumber(-x.real, -x.imaginary)

    def fromInt(x: Int): ComplexNumber = ComplexNumber(x.toDouble, 0)

    def toInt(x: ComplexNumber): Int = x.real.toInt

    def toLong(x: ComplexNumber): Long = x.real.toLong

    def toFloat(x: ComplexNumber): Float = x.real.toFloat

    def toDouble(x: ComplexNumber): Double = x.real

    def compare(x: ComplexNumber, y: ComplexNumber): Int = {
      if (x.real > y.real) 1
      else if (x.real < y.real) -1
      else if (x.imaginary > y.imaginary) 1
      else if (x.imaginary < y.imaginary) -1
      else 0
    }

    override def parseString(str: String): Option[ComplexNumber] = {
      val complexNumberPattern: Regex = """([-+]?\d*\.?\d+) *([-+] *\d*\.?\d+) *i""".r
      str match {
        case complexNumberPattern(r, i) =>
          (r.toDoubleOption, i.toDoubleOption) match {
            case (Some(real), Some(imag)) => Some(ComplexNumber(real, imag))
            case _                        => None
          }
        case _ => None
      }
    }
  }

  implicit class ComplexNumberSyntax(val sc: StringContext) extends AnyVal {
    def z(args: Any*): ComplexNumber = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buf = new StringBuilder(strings.next())
      while (strings.hasNext) {
        buf.append(expressions.next())
        buf.append(strings.next())
      }
      val parts = buf.toString.split("\\+|i")
      ComplexNumber(parts(0).toDouble, parts(1).toDouble)
    }
  }
}
