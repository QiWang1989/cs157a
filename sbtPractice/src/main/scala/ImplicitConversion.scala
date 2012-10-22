/**
 * Created with IntelliJ IDEA.
 * User: qiwang
 * Date: 9/25/12
 * Time: 6:24 PM
 * Compiler will use these implicit conversions to convert parameters into proper types
 */
object ImplicitConversion {
  implicit def intToPN(x: Int):PN = PN(intToList(x))

  implicit def intToList(x: Int):List[Int] = {
    val listX = x.toString toList

    for (i <- listX) yield i.toString.toInt
  }

  implicit def listToPN(x: List[Int]):PN = PN(x)

  implicit def pnToList(x: PN):List[Int] = x.pn
}
