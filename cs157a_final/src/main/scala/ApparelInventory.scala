/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 12:00 PM
 * To change this template use File | Settings | File Templates.
 */

class ApparelInventory extends Inventory with ApparelInventoryDB{

  def cmd = List("Choose the number of operation you want:",
    "1. Add a book to library",
    "2. Delete a book from library",
    "3. Remove one copy of a book from library",
    "4. Borrow",
    "5. Return",
    "0. Quit"
  )

  def cmdInterpretor:PartialFunction[Int, Boolean] = {
    case 1 =>   //Add a book to library
//      adminAddEntry(new LibraryEntry("NO5","Life of Pi", "An Lee"))
      true
    case 2 =>   //Delete a book from library
//      println("Enter book id:")
//      adminDeleteEntry(readLine())
      true
    case 3 =>
      true
    case 4 =>   //Borrow
//      println("Enter book id:")
//      borrowBook("Qi",readLine())
      true
    case 5 =>   //Return
//      println("Enter Book id:")
//      val id = readLine()
//      println("Enter User:")
//      val user = readLine()
//      returnBook(id, user)
      true
  }
}