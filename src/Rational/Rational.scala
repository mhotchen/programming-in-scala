import language.implicitConversions

class Rational(numerator: Int, denominator: Int) extends Ordered[Rational] {
  require(denominator != 0)

  private lazy val gcd = greatestCommonDivisor(numerator.abs, denominator.abs)
  lazy val n = numerator / gcd
  lazy val d = denominator / gcd

  def this(numerator: Int) = this(numerator, 1)

  override def toString = n + "/" + d

  def + (that: Rational) = new Rational(n * that.d + that.n * d, d * that.d)
  def + (i: Int) = new Rational(n + i * d, d)

  def - (that: Rational) = new Rational(n * d - n * d, d * d)
  def - (i: Int) = new Rational(n - i * d, d)

  def * (that: Rational) = new Rational(n * that.n, d * that.d)
  def * (i: Int) = new Rational(n * i, d)

  def / (that: Rational) = new Rational(n * d, d * n)
  def / (i: Int) = new Rational(n, d * i)

  override def compare(that: Rational): Int = (this.n * that.d) - (that.n * this.d)

  def unary_- = new Rational(-n, -d)
  def unary_+ = new Rational(+n, +d)

  def max(that: Rational) = if (this < that) that else this

  private def greatestCommonDivisor(a: Int, b: Int): Int = b match {
    case 0 => a
    case _ => greatestCommonDivisor(b, a % b)
  }
}

implicit def intToRational(x: Int): Rational = new Rational(x)
