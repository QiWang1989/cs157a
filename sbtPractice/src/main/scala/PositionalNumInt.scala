/**
 * Created with IntelliJ IDEA.
 * User: qiwang
 * Date: 9/25/12
 * Time: 5:55 PM
 * Basic numeric operations for positional number system
 */
class PN(val pn:List[Int]) {
  import ImplicitConversion._

  /**
   * Supports both mixed base system and fixed base system
   */
  def +(num2: PN, base: Any = 10): PN = {
    def addRoutinehelper(bb: List[Int]) = {
      var result = List(0)
      // sort two lists from smaller length to bigger length
      val numList = List(pn, num2.pn) sortWith ((x, y) => x.length < y.length)
      // prepend shorter list with 0 to the same length as longer list
      val numWith0 = prependList(numList(0), numList(1).length, 0)
      val extendedBase = prependList(bb, numWith0.length, bb.head)
      // add each digit
      numWith0.indices.reverse foreach (i => result = prependCarryRemainder(numList(1)(i) + numWith0(i) + result.head, extendedBase(i), result))

      if (result.head == 0) result.tail else result
    }

    base match {
      case b: Int => addRoutinehelper(List(b)) //fixed base system
      case b: List[Int] => addRoutinehelper(b) //mixed base system
    }
  }

  /**
   * Supports both mixed base system and fixed base system
   */
  def -(num2: PN, base: Any = 10): (PN, Boolean) = {
    def subRoutineHelper(bb: List[Int]) = {
      val extendedBase = prependList(bb, pn.length, bb.head)
      val extendedNum2 = prependList(num2.pn, pn.length, 0)
      val complement = calComplement(extendedNum2, extendedBase)
      val partialResult = ((this + (1, base)) + (complement, base)).pn  // add complement with num1
      if (partialResult.length == extendedBase.length + 1) (PN(partialResult.tail), true) else (PN(partialResult), false)
    }

    base match {
      case b: Int => subRoutineHelper(List(b)) //fixed base system
      case b: List[Int] => subRoutineHelper(b) //mixed base system
    }
  }

  def *(num2: PN, base: Int = 10): PN = {
    var result = List(0)

    (pn.length - 1 to 0 by -1) foreach {
      i =>
        var mResult = List(0)
        //multiply each digit
        num2.pn.reverseMap(j => mResult = prependCarryRemainder(pn(i) * j + mResult.head, base, mResult))
        // append 0 to partial result, which is the same as multiply base**n
        mResult = mResult.padTo(mResult.length + pn.length - i - 1, 0)
        result = (mResult + (result, base)).pn
    }
    if (result.head == 0) result.tail else result
  }


  def /(num2: PN, b: Int = 10): (PN, PN) = {
    // if num2 only has one digit, simpler division algorithm should be used.
    val n = num2.pn.length
    if (n < 2) return divOneDigit(pn, num2.pn(0), b)

    // D1. Normalization
    val d = b / (num2.pn(0) + 1)
    var u = if (d == 1) 0 :: pn else (this * (d, b)).pn
    val v = num2 * (d, b)
    var q_hat = 0
    var q = List[Int]()
    val m = u.length - n

    (0 until m ) foreach {
      j =>
      // D3. Calculate q_hat
        if (u(j) == v(0)) q_hat = b - 1 else q_hat = (u(j) * b + u(j + 1)) / v(0)

        while (v(1) * q_hat > ((u(j) * b + u(j + 1) - q_hat * v(0)) * b + u(j + 2))) q_hat = q_hat - 1

        //D4. Multiply and subtract
        val subResult = PN(u.slice(j, j + n+1)) - (PN(List(q_hat)) * (v, b), b)
        var u_ = subResult._1

        if (!subResult._2) {                 // if negative
          q = q :+ (q_hat - 1)
          u_ = (PN(v) + (u_, b)).pn.tail
        } else {
          q = q :+ q_hat
        }
        u = u.patch(j, u_, n+1)
    }
    val u_rest = u.slice(m, u.length)
    if (q.head == 0) (q.tail, divOneDigit(u_rest, d, b)._1) else (q, divOneDigit(u_rest, d, b)._1)
  }

  /**
   * save carry as head of result list, and tail is a list of remainder
   */
  def prependCarryRemainder(x: Int, base: Int, result: List[Int]): List[Int] = (x / base) :: (x % base) :: result.tail

  /**
   * save remainder as head of result list, and tail is quotient
   */
  def prependRemainderAppendQuotient(x: Int, v: Int, result: List[Int]): List[Int] = ((x % v) :: result.tail) :+ (x / v)

  /**
   * division routine when v only has one digit
   */
  def divOneDigit(num1: List[Int], v: Int, base: Int = 10): (PN, PN)= {
    var result = List(0)
    num1.map(i => result = prependRemainderAppendQuotient(i + result.head * base, v, result))

    var quotient = result.tail
    if (quotient.head == 0 && quotient.length>1) quotient = quotient.tail
    (PN(quotient), PN(List(result.head)))
  }

  /**
   * calculate b's complement
   */
  def calComplement(x: List[Int], base: List[Int]): List[Int] = {
    var result = List[Int]()
    x.indices.reverse foreach (i => result = (base(i) - 1 - x(i)) :: result)
    result
  }

  def prependList(ls: List[Int], length: Int, padding: Int): List[Int] = List.fill(length - ls.length)(padding) ::: ls

  override def toString = pn.toString()

  def equals(num2:PN):Boolean={
    if (pn.length != num2.pn.length) return false
    (0 until pn.length) foreach { i=> if (pn(i) != num2.pn(i)) return false}
    true
  }
}

object PN{
  def apply(n:List[Int]) = new PN(n)
}