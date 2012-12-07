/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 11:56 AM
 * To change this template use File | Settings | File Templates.
 */
trait Inventory {
  self:InventoryDB =>

  val genCmd:List[String] = List(
    "0. Quit",
    "101. List"
  )

  def cmd:List[String]

//  def availableInventory:List[String] = List(
//    "1. Library",
//    "2. Apparel")

  def cmdLoop = {
    var continue = true
//    println("Choose inventory system:\n"+availableInventory.mkString("\n"))

    while(continue){
      println(cmd.mkString("\n"))

      val c = readInt()
      continue = (cmdInterpretor orElse generalCmd)(c)
    }
  }

  def generalCmd:PartialFunction[Int, Boolean] = {
    case 0 => false
    case 101 => list; true
    case _ => println("Invalid command"); true
  }


  def cmdInterpretor:PartialFunction[Int, Boolean]
}
