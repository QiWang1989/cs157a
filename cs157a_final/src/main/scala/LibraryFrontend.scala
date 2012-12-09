/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 12:00 PM
 * To change this template use File | Settings | File Templates.
 */

class LibraryFrontend extends Frontend with LibraryInventoryDB{

  val cmd= genCmd:::List(
    "l1. Borrow",
    "l2. Return"
  )

  def cmdInterpretor:PartialFunction[String, Boolean] = {
    case "1" =>   //Add a book to library
      println("Enter book id:")
      val id = readLine()
      println("Enter book title:")
      val title = readLine()
      println("Enter Author:")
      val author = readLine()
      println("Enter Anumber of copy:")
      val copy= readInt()
      add(LibraryEntry(id,title, author), copy)
      true
    case "l1" =>   //Borrow
      println("Enter book id:")
      val id = readLine()
      println("Enter User:")
      val user = readLine()
      borrowBook(id,user)
      true
    case "l2" =>   //Return
      println("Enter Book id:")
      val id = readLine()
      println("Enter User:")
      val user = readLine()
      returnBook(id, user)
      true
  }
}