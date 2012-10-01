/**
 * Created with IntelliJ IDEA.
 * User: qiwang
 * Date: 9/25/12
 * Time: 6:24 PM
 * Compiler will use these implicit conversions to convert parameters into proper types
 */
object ImplicitConversion {
  implicit def intToPN(x: Int):PN = {
    var listX = List[Int]()
    x.toString foreach (i => listX = listX :+ Integer.parseInt(i.toString, 10))
    PN(listX)
  }

  implicit def intToList(x: Int):List[Int] = {
    var listX = List[Int]()
    x.toString foreach (i => listX = listX :+ Integer.parseInt(i.toString, 10))
    listX
  }

  implicit def listToPN(x: List[Int]):PN = PN(x)

  implicit def pnToList(x: PN):List[Int] = x.pn
}
