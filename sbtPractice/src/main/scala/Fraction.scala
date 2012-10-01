import annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: qiwang
 * Date: 9/25/12
 * Time: 6:32 PM
 * Fraction operations for positional number system
 */
class Fraction(val n: PN, val d: PN) {
  import ImplicitConversion._

  require(!(d equals PN(0)))

  def this(n: PN) = this(n, PN(List(1)))

  def +(r:Fraction) = {
    Fraction(n*r.d+r.n*d,d*r.d).simplify
  }

  def -(r:Fraction) = {
    Fraction((n*r.d-r.n*d)._1,d*r.d).simplify
  }

  def *(r:Fraction) = {
    Fraction(n*r.n,d*r.d).simplify
  }

  def /(r:Fraction) = {
    Fraction(n*r.d,d*r.n).simplify
  }

  def equals(f:Fraction):Boolean = ((n equals f.n) && (d equals f.d))

  private def simplify:Fraction = {
    val g = gcd(n,d)
    new Fraction((n/g)._1, (d/g)._1)
  }

  @tailrec
  private def gcd(x:PN, y:PN):PN = if (y equals PN(0)) x else gcd(y, (x/y)._2)

  override def toString = n + "/" + d
}


object Fraction{
  def apply(n:PN) = new Fraction(n)

  def apply(n: PN, d: PN)={
    new Fraction(n, d).simplify
  }
}