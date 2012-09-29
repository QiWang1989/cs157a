import annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: qiwang
 * Date: 9/25/12
 * Time: 6:32 PM
 * To change this template use File | Settings | File Templates.
 */
class Fraction(val n: Int, val d: Int) {
  require(d != 0)

  override def toString = n + "/" + d

  def this(n: Int) = this(n, 1)

  /**
   * [[]]
   * @param r Fraction
   * @return Unit
   */
  def +(r:Fraction) = {
    Fraction(n*r.d+r.n*d,d*r.d).simplify
  }

  def -(r:Fraction) = {
    Fraction(n*r.d-r.n*d,d*r.d).simplify
  }

  def *(r:Fraction) = {
    Fraction(n*r.n,d*r.d).simplify
  }

  def /(r:Fraction) = {
    Fraction(n*r.d,d*r.n).simplify
  }

  def equal(f:Fraction):Boolean = (n == f.n && d == f.d)

  private def simplify:Fraction = {
    val g = gcd(n,d)
    new Fraction(n/g, d/g)
  }

  @tailrec
  private def gcd(x:Int, y:Int):Int = if (y==0) x else gcd(y, x%y)
}


object Fraction{
  def apply(n:Int) = new Fraction(n)

  def apply(n: Int, d: Int)={
    new Fraction(n, d).simplify
  }
}