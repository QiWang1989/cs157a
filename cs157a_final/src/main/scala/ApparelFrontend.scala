/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 12:00 PM
 * To change this template use File | Settings | File Templates.
 */

class ApparelFrontend extends Frontend with ApparelInventoryDB{
  val cmd= genCmd

  def cmdInterpretor:PartialFunction[String, Boolean] = {
    case "1" =>   //Add a book to library
      println("Enter id:")
      val id = readLine()
      println("Enter style:")
      val style = readLine()
      println("Enter location:")
      val location = readLine()
      println("Enter Anumber of copy:")
      val copy= readInt()
      add(ApparelEntry(id,style, location), copy)
      true
  }
}