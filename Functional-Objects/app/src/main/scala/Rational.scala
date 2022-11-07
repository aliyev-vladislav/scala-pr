class Rational(n: Int, d: Int):

  require(d != 0)

  private val g = gcd(n.abs, d.abs)
  val numer: Int = n / g
  val denom: Int = d / g

  def this(n: Int) = this(n, 1)

  override def toString = s"$numer/$denom"

  def + (that: Rational): Rational =
    Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def - (that: Rational): Rational =
    Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def - (i: Int): Rational =
    Rational(numer - i * denom, denom)

  def * (that: Rational): Rational =
    Rational(numer * that.numer, denom * that.denom)

  def * (i: Int): Rational =
    Rational(numer * i, denom)

  def / (that: Rational): Rational =
    Rational(numer * that.denom, denom * that.numer)

  def / (i: Int): Rational =
    Rational(numer, denom * i)

  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)

extension (x: Int)
  def +(y: Rational) = Rational(x) + y
  def -(y: Rational) = Rational(x) - y
  def *(y: Rational) = Rational(x) * y
  def /(y: Rational) = Rational(x) / y

@main def m() =
  val x = Rational(3, 2)
  val y = Rational(3, 2)
  val result = 2 * x
  println(result)

