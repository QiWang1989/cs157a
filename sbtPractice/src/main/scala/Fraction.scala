import annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: qiwang
 * Date: 9/25/12
 * Time: 6:32 PM
 * Fraction operations for both positional number system and native language number system
 */
abstract class Fraction[U](val n: U, val d: U) {
  type T <: Fraction[U]

  def +(r: T): T

  def -(r: T): T

  def *(r: T): T

  def /(r: T): T

  def equals(r: T): Boolean
  def simplify: T

  override def toString = n + "/" + d
}

class PNFraction(override val n: PN, override val d: PN) extends Fraction(n, d) {
  require(!(d equals 0))

  type T = PNFraction

  def this(n: PN) = this(n, PN(List(1)))

  def +(r: T): T = {
    new PNFraction(n * r.d + r.n * d, d * r.d).simplify
  }

  def -(r: PNFraction): PNFraction = {
    new PNFraction(n * r.d - r.n * d, d * r.d).simplify
  }

  def *(r: T): PNFraction = {
    new PNFraction(n * r.n, d * r.d).simplify
  }

  def /(r: PNFraction): PNFraction = {
    new PNFraction(n * r.d, d * r.n).simplify
  }

  def equals(f: PNFraction): Boolean = ((n equals f.n) && (d equals f.d))

  def simplify: PNFraction = {
    val g = gcd(n, d)
    new PNFraction(n / g, d / g)
  }

  @tailrec
  final def gcd(x: PN, y: PN): PN = {
    if (y equals PN(List(0))) x else gcd(y, x % y)
  }
}


class NativeFraction(override val n: Int, override val d: Int) extends Fraction(n, d) {
  require(!(d equals 0))

  type T = NativeFraction

  def this(n: Int) = this(n, 1)

  override def +(r: NativeFraction): NativeFraction = {
    new NativeFraction(n * r.d + r.n * d, d * r.d).simplify
  }

  override def -(r: NativeFraction): NativeFraction = {
    new NativeFraction(n * r.d - r.n * d, d * r.d).simplify
  }

  override def *(r: NativeFraction): NativeFraction = {
    new NativeFraction(n * r.n, d * r.d).simplify
  }

  override def /(r: NativeFraction): NativeFraction = {
    new NativeFraction(n * r.d, d * r.n).simplify
  }

  def equals(f: NativeFraction): Boolean = ((n equals f.n) && (d equals f.d))

  def simplify: NativeFraction = {
    val g = gcd(n, d)
    new NativeFraction(n / g, d / g)
  }

  @tailrec
  final def gcd(x: Int, y: Int): Int = {
    if (y == 0) x else gcd(y, x % y)
  }
}

object Fraction {
  def apply(n: PN, d: PN) = {
    new PNFraction(n, d).simplify
  }

  def apply(n: Int, d: Int) = {
    new NativeFraction(n, d).simplify
  }
}