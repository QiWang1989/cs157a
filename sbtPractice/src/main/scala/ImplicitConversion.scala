/**
 * Created with IntelliJ IDEA.
 * User: qiwang
 * Date: 9/25/12
 * Time: 6:24 PM
 * To change this template use File | Settings | File Templates.
 */
object ImplicitConversion {
  /**
   * Purpose of this implicit conversion function is when I provide add() and mul() with Int type, it'll be converted to List[] automatically
   * This only works when base is less than 10
   */
  implicit def intToPN(x: Int):PN = {
    var listX = List[Int]()
    x.toString foreach (i => listX = listX :+ Integer.parseInt(i.toString, 10))
    PN(listX)
  }

  implicit def listToPN(x: List[Int]):PN = {
    PN(x)
  }
  implicit def pnToList(x: PN):List[Int] = {
    x.pn
  }
}
