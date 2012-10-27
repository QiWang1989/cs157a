import annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: qiwang
 * Date: 9/25/12
 * Time: 6:32 PM
 * Fraction operations for positional number system
 */
trait BaseNum {
//  type U <: {
    def +[U<:Any](x: U): U
    def *[U<:Any](x: U): U
//  }
}

//trait PNNum extends BaseNum{
//  type U = PN
//}
//
//trait IntNum extends BaseNum{
//  type U = Int
//}

abstract class Fraction[U<:{
  def +[M<:Any](x: M): M
  def *[M<:Any](x: M): M
}] {
//  this: BaseNum =>
  type T <: Fraction[U]
  val n: U
  val d: U

  def makeInstance(x: U, y: U): T

  def +(r: T): T = makeInstance(n * r.d + r.n * d, d * r.d)
  def -(r:T):T
  def *(r: T): T = makeInstance(n * r.n, d * r.d)
  def /(r:T):T
}

class PNFraction(val n: PN, val d: PN) extends Fraction[PN]{

  import ImplicitConversion._

  require(!(d equals 0))

  type T = PNFraction
//  override type U = PN

  def makeInstance(x: PN, y: PN): T = new PNFraction(x, y).simplify

  def this(n: PN) = this(n, PN(List(1)))

    def +(r:T):T = {
      super.+(r).simplify
    }

  def -(r: PNFraction): PNFraction = {
    Fraction(n * r.d - r.n * d, d * r.d).simplify
  }

    def *(r:T):PNFraction = {
      Fraction(n*r.n,d*r.d).simplify
    }

  def /(r: PNFraction): PNFraction = {
    Fraction(n * r.d, d * r.n).simplify
  }

  def equals(f: PNFraction): Boolean = ((n equals f.n) && (d equals f.d))

  def simplify: PNFraction = {
    val g = gcd(n, d)
    new PNFraction(n / g, d / g)
  }

  @tailrec
  private def gcd(x: PN, y: PN): PN = {
    if (y equals PN(List(0))) x else gcd(y, x % y)
  }

  override def toString = n + "/" + d
}


class NativeFraction(val n: Int, val d: Int) extends Fraction[Int]{

  require(!(d equals 0))

  override type T = NativeFraction
//  override type U = Int

  def this(n: Int) = this(n, 1)

  def makeInstance(x: Int, y: Int): T = new NativeFraction(x, y).simplify

  override def +(r: NativeFraction): NativeFraction = {
    Fraction(n*r.d+r.n*d,d*r.d).simplify
  }

  override def -(r: NativeFraction): NativeFraction = {
    Fraction(n * r.d - r.n * d, d * r.d).simplify
  }

  override def *(r: NativeFraction): NativeFraction = {
    Fraction(n * r.n, d * r.d).simplify
  }

  override def /(r: NativeFraction): NativeFraction = {
    Fraction(n * r.d, d * r.n).simplify
  }

  def equals(f: NativeFraction): Boolean = ((n equals f.n) && (d equals f.d))

  def simplify: NativeFraction = {
    val g = gcd(n, d)
    new NativeFraction(n / g, d / g)
  }

  @tailrec
  private def gcd(x: Int, y: Int): Int = {
    if (y == 0) x else gcd(y, x % y)
  }

  override def toString = n + "/" + d
}

object Fraction {
  //  def apply(n:PN) = new PNFraction(n)

  def apply(n: PN, d: PN) = {
    new PNFraction(n, d).simplify
  }

  def apply(n: Int, d: Int) = {
    new NativeFraction(n, d).simplify
  }
}