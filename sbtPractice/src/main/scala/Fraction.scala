import annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: qiwang
 * Date: 9/25/12
 * Time: 6:32 PM
 * Fraction operations for both positional number system and native language number system
 */
class Fraction[T<:Arithmetic[T]](val n: T, val d: T) extends Arithmetic[Fraction[T]]{
  require(!(d isZero))

  def +(r: Fraction[T]): Fraction[T] = {
    new Fraction(n * r.d + r.n * d, d * r.d).simplify
  }

  def -(r: Fraction[T]): Fraction[T] = {
    new Fraction(n * r.d - r.n * d, d * r.d).simplify
  }

  def *(r: Fraction[T]): Fraction[T]= {
    new Fraction(n * r.n, d * r.d).simplify
  }

  def /(r: Fraction[T]): Fraction[T] = {
    new Fraction(n * r.d, d * r.n).simplify
  }

  def %(r: Fraction[T]): Fraction[T] = null

  override def equals(other: Any): Boolean = {
    val f = other.asInstanceOf[Fraction[T]]
    ((n equals f.n) && (d equals f.d))
  }

  def simplify: Fraction[T] = {
    val g = gcd(n, d)
    new Fraction(n / g, d / g)
  }

  override def toString = n + "/" + d

  @tailrec
  final def gcd(x: T, y: T): T = {
    if (y isZero) x else gcd(y, x % y)
  }

  def isZero:Boolean = (n isZero)
}

object Fraction {
  def apply[U<:Arithmetic[U]](n: U, d: U) = {
    new Fraction(n, d).simplify
  }
}