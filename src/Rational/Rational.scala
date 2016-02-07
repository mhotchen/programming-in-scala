import language.implicitConversions

class Rational(numerator: Int, denominator: Int) {
  require(denominator != 0)

  private val gcd = greatestCommonDivisor(numerator.abs, denominator.abs)
  val n = numerator / gcd
  val d = denominator / gcd

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

  def < (that: Rational) = n * that.d < that.n * d
  def > (that: Rational) = if (this < that) false else true

  def unary_- = new Rational(-n, -d)
  def unary_+ = new Rational(+n, +d)

  def max(that: Rational) = if (this < that) that else this

  private def greatestCommonDivisor(a: Int, b: Int): Int = if (b == 0) a else greatestCommonDivisor(b, a % b)
}

implicit def intToRational(x: Int): Rational = new Rational(x)
