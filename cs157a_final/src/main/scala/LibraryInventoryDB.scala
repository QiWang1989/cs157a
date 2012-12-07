/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 12:00 PM
 * To change this template use File | Settings | File Templates.
 */


import org.squeryl.SessionFactory
import org.squeryl.PrimitiveTypeMode._

case class LibraryEntry(bookId: String, title: String, author: String, copyInStock: Int, totalCopy: Int) extends Entry {
  def this(bookId: String, title: String, author: String) = this(bookId, title, author, 1, 1)
  override def toString = bookId+"\t"+title+"\t"+author+"\t"+copyInStock+"\t"+totalCopy
}

case class Borrow(bookId: String, user: String) extends Entry

object Library extends org.squeryl.Schema {
  val book = table[LibraryEntry]("book")
  val borrow = table[Borrow]("borrow")
}

trait LibraryInventoryDB extends InventoryDB {
  type T = LibraryEntry

  // Add a book entry to library (either initial copy or additional copy)
  // increase both copyInStock and totalCopy
  def add(entry: T) = executeQuery("library") {
    ifBookExist(entry.bookId) match {
      case Some(book) =>
        addOneCopyHelper(entry.bookId)
        println("Add another copy to <" + entry.title + ">")
      case None =>
        println("Inserted entry:" + Library.book.insert(entry))
    }
  }

  // remove all copies of this book
  def delete(id: String) = executeQuery("library") {
    Library.book.deleteWhere(book => book.bookId === id) match {
      case 1 => println("Entry deleted successfully.")
      case 0 => println("Entry doesn't exist.")
      case _ => println("Error.")
    }
  }

  // add one additional copy
  // increase both copyInStock and totalCopy
  def addOneCopy(id: String) = executeQuery("library") {
    addOneCopyHelper(id)
  }

  // increase both copyInStock and totalCopy
  def addOneCopyHelper(id: String) = {
    update(Library.book)(bk =>
      where(bk.bookId === id)
        set(bk.copyInStock := bk.copyInStock.~ + 1,
        bk.totalCopy := bk.totalCopy.~ + 1)
    )
  }

  //delete one copy from library
  //decrease both copyInStock and totalCopy
  def deleteOneCopy(id: String) = executeQuery("library") {
    val result = (from(Library.book)(bk => where(bk.bookId === id) select (bk)))
    result.isEmpty match {
      case false if result forall (_.copyInStock > 0) =>
        update(Library.book)(bk =>
          where(bk.bookId === id)
            set(bk.copyInStock := bk.copyInStock.~ - 1,
            bk.totalCopy := bk.totalCopy.~ - 1)
        )
        println("Delete one copy of <" + id + ">")
      case false if result forall (_.copyInStock == 0) =>
        println("Out of stock")
      case true =>
        println("Library doesn't have this book.")
    }
  }

  def borrowBook(id: String, user: String) = executeQuery("library") {
    ifBookExist(id) match {
      case Some(book) if book.copyInStock > 0 =>
        update(Library.book)(bk =>
          where(bk.bookId === id)
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

  def returnBook(id: String, user: String) = executeQuery("library") {
    ifBookExist(id) match {
      case Some(book) =>
        update(Library.book)(bk =>
          where(bk.bookId === id)
            set (bk.copyInStock := bk.copyInStock.~ + 1)
        )

        Library.borrow.deleteWhere(borrow => borrow.user === user and borrow.bookId === id) match {
          case 1 => println("Entry deleted successfully.")
          case 0 => println("Entry doesn't exist.")
          case _ => println("Error.")
        }
      case None =>
        println("This book is not own by the library")
    }
  }

  // if book exists in Book table, then return that row; otherwise return None
  def ifBookExist(id: String): Option[LibraryEntry] = {
    var result: org.squeryl.Query[LibraryEntry] = null
    inTransaction {
      result = from(Library.book)(bk => where(bk.bookId === id) select (bk))
    }

    result.isEmpty match {
      case false => Some(result.head)
      case true => None
    }
  }

  def list =  executeQuery("library"){
    val result = from(Library.book)(bk=>select(bk))

    result.isEmpty match{
      case false => result.foreach(println(_))
      case true => println("Empty inventory")
    }
  }
}
