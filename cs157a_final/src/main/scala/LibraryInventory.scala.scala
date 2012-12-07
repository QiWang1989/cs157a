/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 12:00 PM
 * To change this template use File | Settings | File Templates.
 */

class LibraryInventory extends Inventory with LibraryInventoryDB{

  val cmd= List("Choose the number of operation you want:",
    "1. Add a book to library",
    "2. Delete a book from library",
    "3. Remove one copy of a book from library",
    "4. Borrow",
    "5. Return"
  ):::genCmd

  def cmdInterpretor:PartialFunction[Int, Boolean] = {
    case 1 =>   //Add a book to library
      println("Enter book id:")
      val id = readLine()
      println("Enter book title:")
      val title = readLine()
      println("Enter Author:")
      val author = readLine()
      add(new LibraryEntry(id,title, author))
//      adminAddEntry(new LibraryEntry("NO5","Life of Pi", "An Lee"))
      true
    case 2 =>   //Delete a book from library
      println("Enter book id:")
      delete(readLine())
      true
    case 3 =>
      println("Enter book id:")
      deleteOneCopy(readLine())
      true
    case 4 =>   //Borrow
      println("Enter book id:")
      val id = readLine()
      println("Enter User:")
      val user = readLine()
      borrowBook(id,user)
      true
    case 5 =>   //Return
      println("Enter Book id:")
      val id = readLine()
      println("Enter User:")
      val user = readLine()
      returnBook(id, user)
      true
  }
}