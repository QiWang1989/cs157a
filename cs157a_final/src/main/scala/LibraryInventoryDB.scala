/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 12:00 PM
 * To change this template use File | Settings | File Templates.
 */


import org.squeryl.{Table, SessionFactory}
import org.squeryl.PrimitiveTypeMode._

case class LibraryEntry(override val id: String, title: String, author: String, override val copyInStock: Int=1, override val totalCopy: Int=1)
  extends Entry(id, copyInStock, totalCopy){
  def setCopy(copy:Int):LibraryEntry = LibraryEntry(this.id, this.title, this.author, copy, copy)
  override def toString = id+"\t"+title+"\t"+author+"\t"+copyInStock+"\t"+totalCopy
}

case class Borrow(id: String, user: String)

object Library extends org.squeryl.Schema {
  val book = table[LibraryEntry]("book")
  val borrow = table[Borrow]("borrow")
}

trait LibraryInventoryDB extends InventoryDB {
  type T = LibraryEntry
  def mainTable = Library.book
  def database="library"

  def borrowBook(id: String, user: String) = executeQuery{
    ifExist(id) match {
      case Some(book) if book.copyInStock > 0 =>
        update(Library.book)(bk =>
          where(bk.id === id)
            set (bk.copyInStock := bk.copyInStock.~ - 1)
        )
        try {
          Library.borrow.insert(Borrow(id, user))
        } catch {
          case e => println("This user has already borrowed the same book.")
        }
      case Some(book) if book.copyInStock == 0 =>
        println("Out of stock")
      case None =>
        println("Library doesn't carry this book")
    }
  }

  def returnBook(id: String, user: String) = executeQuery{
    ifExist(id) match {
      case Some(book) =>
        update(Library.book)(bk =>
          where(bk.id === id)
            set (bk.copyInStock := bk.copyInStock.~ + 1)
        )

        Library.borrow.deleteWhere(borrow => borrow.user === user and borrow.id === id) match {
          case 1 => println("Entry deleted successfully.")
          case 0 => println("Entry doesn't exist.")
          case _ => println("Error.")
        }
      case None =>
        println("This book is not own by the library")
    }
  }
}
