/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 11:56 AM
 * To change this template use File | Settings | File Templates.
 */
trait Frontend {
  self:InventoryDB =>

  val genCmd:List[String] = List("Choose the number of operation you want:",
    "0. Quit",
    "1. Add ",
    "2. Delete a book from library",
    "3. Remove copies of a book from library",
    "4. List",
    "5. Add copy to existing book"
  )

  def cmd:List[String]

  def cmdLoop = {
    var continue = true

    while(continue){
      println(cmd.mkString("\n"))

      val c = readLine()
      continue = (generalCmd orElse cmdInterpretor)(c)
    }
  }

  def generalCmd:PartialFunction[String, Boolean] = {
    case "0" => false
    case "2" =>   //Delete a book from library
      println("Enter book id:")
      delete(readLine())
      true
    case "3" =>  //Delete specified number of copies
      println("Enter book id:")
      val id = readLine()
      println("Enter number of copy:")
      val copy = readInt()
      deleteCopy(id,copy)
      true
    case "4" =>
      println("Enter book id:")
      list(readLine())
      true
    case "5" =>
      println("Enter book id:")
      val id = readLine()
      println("Enter number of copy:")
      val copy = readInt()
      addCopy(id, copy)
      true
  }


  def cmdInterpretor:PartialFunction[String, Boolean]
}
