import annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: qiwang
 * Date: 9/25/12
 * Time: 6:32 PM
 * Fraction operations for both positional number system and native language number system
 */
class Fraction[T <: Arithmetic[T]](val n: T, val d: T) extends Arithmetic[Fraction[T]] {
  require(!(d isZero))

  def +(r: Fraction[T]): Fraction[T] = {
    Fraction(n * r.d + r.n * d, d * r.d)
  }

  def -(r: Fraction[T]): Fraction[T] = {
    Fraction(n * r.d - r.n * d, d * r.d)
  }

  def *(r: Fraction[T]): Fraction[T] = {
    Fraction(n * r.n, d * r.d)
  }

  def /(r: Fraction[T]): Fraction[T] = {
    Fraction(n * r.d, d * r.n)
  }

  def %(r: Fraction[T]): Fraction[T] = null

  override def equals(other: Any): Boolean = {
    val f = other.asInstanceOf[Fraction[T]]
    ((n equals f.n) && (d equals f.d))
  }


  override def toString = n + "/" + d


  def isZero: Boolean = (n isZero)
}

object Fraction {
  def apply[T <: Arithmetic[T]](n: T, d: T) = {
    def gcd(x: T, y: T): T = {
      if (y isZero) x else gcd(y, x % y)
    }

    val g = gcd(n, d)
    val (nn, dd) = (n / g, d / g)

    new Fraction(nn, dd)
  }
}