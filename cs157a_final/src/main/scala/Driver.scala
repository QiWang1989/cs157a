/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 11:56 AM
 * To change this template use File | Settings | File Templates.
 */
object Driver extends App{
  println("Choose inventory system:")
  println("1. Library")
  println("2. Apparel")

  val choice = readLine()
  choice.toLowerCase match {
    case "library" =>
      val continue = true
      val inventory = new Inventory with LibraryInventoryDB

      while(continue){
        println("Choose the number of operation you want:")
        println("1. Add a book to library")
        println("2. Delete a book from library")
        println("3. Remove one copy of a book from library")
        println("4. Borrow")
        println("5. Return")

        val cmd = readInt()

        cmd match {
          case 1 =>   //Add a book to library
            inventory.adminAddEntry(new LibraryEntry("NO5","Life of Pi", "An Lee"))
          case 2 =>   //Delete a book from library
            println("Enter book id:")
            inventory.adminDeleteEntry(readLine())
          case 3 =>

          case 4 =>   //Borrow
            println("Enter book id:")
            inventory.borrowBook("Qi",readLine())
          case 5 =>   //Return
            println("Enter Book id:")
            val id = readLine()
            println("Enter User:")
            val user = readLine()
            inventory.returnBook(id, user)
          case _ => "Invalid command"
        }
      }
    case "apparel" =>
      val inventory = new Inventory with ApparelInventoryDB
      inventory.adminAddEntry(ApparelEntry("NO1","Shorts", "A&F"))
  }
}
