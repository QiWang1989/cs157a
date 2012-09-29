/**
 * Created with IntelliJ IDEA.
 * User: qiwang
 * Date: 9/25/12
 * Time: 5:55 PM
 * To change this template use File | Settings | File Templates.
 */
class PositionNum {
  import ImplicitConversion._

  /**
   * Example:
   *
   * prependCarryRemainder(14, 10, List(0)) will return List(1, 4)
   *
   * prependCarryRemainder(14, 10, List(2, 5)) will return List(1, 4, 5)
   */
  def prependCarryRemainder(x: Int, base: Int, result: List[Int]): List[Int] = (x / base) :: (x % base) :: result.tail

  def prependRemainderAppendQuotient(x: Int, v: Int, result: List[Int]): List[Int] = ((x % v) :: result.tail) :+ (x / v)

  def divOneDigit(num1: List[Int], v: Int, base: Int = 10): (List[Int], List[Int])= {
    var result = List(0)
    num1.map(i => result = prependRemainderAppendQuotient(i + result.head * base, v, result))

    var quotient = result.tail
    if (quotient.head == 0) quotient = quotient.tail
    (quotient, List(result.head))
  }

  def calComplement(x: List[Int], base: List[Int]): List[Int] = {
    var result = List[Int]()
    x.indices.reverse foreach (i => result = (base(i) - 1 - x(i)) :: result)
    result
  }

  def prependList(ls: List[Int], length: Int, padding: Int): List[Int] = List.fill(length - ls.length)(padding) ::: ls

  /**
   * Supports both mixed base system and fixed base system
   */
  def add(num1: List[Int], num2: List[Int], base: Any = 10): List[Int] = {
    def helper(bb: List[Int]) = {
      var result = List(0)
      val numList = List(num1, num2) sortWith ((x, y) => x.length < y.length) // sort two lists from smaller length to bigger length
      val numWith0 = prependList(numList(0), numList(1).length, 0) // prepend shorter list with 0 to the same length as longer list
      val extendedBase = prependList(bb, numWith0.length, bb.head)
      numWith0.indices.reverse foreach (i => result = prependCarryRemainder(numList(1)(i) + numWith0(i) + result.head, extendedBase(i), result)) // add each digit
      if (result.head == 0) result.tail else result
    }

    base match {
      case b: Int => helper(List(b)) //fixed base system
      case b: List[Int] => helper(b) //mixed base system
    }
  }

  /**
   * Supports both mixed base system and fixed base system
   */
  def subtract(num1: List[Int], num2: List[Int], base: Any = 10): (List[Int], Boolean) = {
    def helper(bb: List[Int]) = {
      val extendedBase = prependList(bb, num1.length, bb.head)
      val extendedNum2 = prependList(num2, num1.length, 0)
      val complement = calComplement(extendedNum2, extendedBase)
      val partialResult = add(add(num1, 1, base), complement, base)  // add complement with num1
      if (partialResult.length == extendedBase.length + 1) (partialResult.tail, true) else (partialResult, false)
    }

    base match {
      case b: Int => helper(List(b)) //fixed base system
      case b: List[Int] => helper(b) //mixed base system
    }
  }

  def mul(num1: List[Int], num2: List[Int], base: Int = 10): List[Int] = {
    var result = List(0)

    (num1.length - 1 to 0 by -1) foreach {
      i =>
        var mResult = List(0)
        num2.reverseMap(j => mResult = prependCarryRemainder(num1(i) * j + mResult.head, base, mResult)) //multiply each digit
        mResult = mResult.padTo(mResult.length + num1.length - i - 1, 0) // append 0 to partial result, which is the same as multiply base**n
        result = add(mResult, result, base)
    }
    if (result.head == 0) result.tail else result
  }


  def longDiv(num1: List[Int], num2: List[Int], b: Int = 10): (List[Int], List[Int]) = {
    // D1. Normalization
    val d = b / (num2(0) + 1)
    var u = if (d == 1) 0 :: num1 else mul(num1, d, b)
    val v = mul(num2, d, b)
    var q_hat = 0
    val n = num2.length
    var q = List[Int]()
    val m = u.length - n

    if (n < 2) return divOneDigit(num1, v(0), b)

    (0 until m ) foreach {
      j =>
      // D3. Calculate q_hat
        if (u(j) == v(0)) q_hat = b - 1 else q_hat = (u(j) * b + u(j + 1)) / v(0)

        while (v(1) * q_hat > ((u(j) * b + u(j + 1) - q_hat * v(0)) * b + u(j + 2))) q_hat = q_hat - 1

        //D4. Multiply and subtract
        val subResult = subtract(u.slice(j, j + n+1), mul(q_hat, v, b), b)
        var u_ = subResult._1

        if (!subResult._2) {                 // if negative
          q = q :+ (q_hat - 1)
          u_ = add(v, u_, b).tail
        } else {
          q = q :+ q_hat
        }
        u = u.patch(j, u_, n+1)
    }
    val u_rest = u.slice(m, u.length)
    if (q.head == 0) (q.tail, divOneDigit(u_rest, d, b)._1) else (q, divOneDigit(u_rest, d, b)._1)
  }
}
