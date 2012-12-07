/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 11:56 AM
 * To change this template use File | Settings | File Templates.
 */
trait Inventory {
  self:InventoryDB =>

  def cmd:List[String]

  def cmdLoop = {
    var continue = true

    while(continue){
      println(cmd.mkString("\n"))

      val c = readInt()
      continue = (cmdInterpretor orElse generalCmd)(c)
    }
  }

  def generalCmd:PartialFunction[Int, Boolean] = {
    case 0 => false
    case _ => "Invalid command"; true
  }


  def cmdInterpretor:PartialFunction[Int, Boolean]

  def adminAddEntry(entry: T) ={
    add(entry)
  }

  // remove all copies of this book
  def adminDeleteEntry(id: String) ={
    delete(id)
  }
}
