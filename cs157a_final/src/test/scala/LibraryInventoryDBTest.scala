/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/5/12
 * Time: 9:46 PM
 * To change this template use File | Settings | File Templates.
 */
import org.scalatest.{Tag, FunSpec}
import org.scalatest.junit.JUnitRunner
import org.squeryl.SessionFactory
import org.squeryl.PrimitiveTypeMode._

@org.junit.runner.RunWith(classOf[JUnitRunner])
class LibraryInventoryDBTest extends FunSpec{
  describe("LibraryInventoryDB ") {
    val libraryInventoryDB = new LibraryInventoryDB{}

    it("add(entry: T) should add one book to the library"){
      libraryInventoryDB.add(new LibraryEntry("uniqueNo1","Life of Pi", "An Lee"))

      libraryInventoryDB.executeQuery("library"){
        assert(!(from(Library.book)(bk => where(bk.bookId === "uniqueNo1") select (bk))).isEmpty)
      }
      libraryInventoryDB.delete("uniqueNo1")
    }

    it("delete(id: String) should delete one book entry from the library"){
      libraryInventoryDB.add(new LibraryEntry("uniqueNo1","Life of Pi", "An Lee"))
      libraryInventoryDB.delete("uniqueNo1")
      libraryInventoryDB.executeQuery("library"){
        assert((from(Library.book)(bk => where(bk.bookId === "uniqueNo1") select (bk))).isEmpty)
      }
    }

    //test-only LibraryInventoryDBTest -- include(borrow)
    it("borrowBook(user: String, id: String) should delete one book entry from the library", Tag("borrow")){
      libraryInventoryDB.add(new LibraryEntry("uniqueNo2","Life of Pi", "An Lee"))
      libraryInventoryDB.borrowBook("uniqueNo2", "Qi")

      libraryInventoryDB.executeQuery("library"){
        from(Library.book)(bk => where(bk.bookId === "uniqueNo2") select (bk.copyInStock)) foreach(_==0)
        assert(!(from(Library.borrow)(borrow => where(borrow.user === "Qi" and borrow.bookId === "uniqueNo2") select (borrow))).isEmpty)
        Library.borrow.deleteWhere(borrow => borrow.user === "Qi" and borrow.bookId === "uniqueNo2")
      }
      libraryInventoryDB.delete("uniqueNo2")
    }

    it("returnBook(user: String, id: String) should delete one copy from book entry record", Tag("return")){
      libraryInventoryDB.add(new LibraryEntry("uniqueNo3","Life of Pi", "An Lee"))
      libraryInventoryDB.borrowBook("uniqueNo3","Qi")
      libraryInventoryDB.returnBook("uniqueNo3","Qi")

      libraryInventoryDB.executeQuery("library"){
        from(Library.book)(bk => where(bk.bookId === "uniqueNo3") select (bk.copyInStock)) foreach(copy=>assert(copy==1))
        assert((from(Library.borrow)(borrow => where(borrow.bookId === "uniqueNo3" and borrow.user === "Qi") select (borrow))).isEmpty)
      }
      libraryInventoryDB.delete("uniqueNo3")
    }

    it("addOneCopy(id:String) should add one copy of an existing book to the library"){
      libraryInventoryDB.add(new LibraryEntry("uniqueNo4","Life of Pi", "An Lee"))
      libraryInventoryDB.addOneCopy("uniqueNo4")

      libraryInventoryDB.executeQuery("library"){
        assert((libraryInventoryDB.ifBookExist("uniqueNo4").get).copyInStock == 2 )
      }
      libraryInventoryDB.delete("uniqueNo4")
    }

    it("deleteOneCopy(id: String) should delete one copy of an existing book from library book table"){
      libraryInventoryDB.add(new LibraryEntry("uniqueNo5","Life of Pi", "An Lee"))
      libraryInventoryDB.addOneCopy("uniqueNo5")
      libraryInventoryDB.deleteOneCopy("uniqueNo5")

      libraryInventoryDB.executeQuery("library"){
        val book = libraryInventoryDB.ifBookExist("uniqueNo5").get
        assert(book.copyInStock == 1 )
        assert(book.totalCopy == 1 )
      }
      libraryInventoryDB.delete("uniqueNo5")
    }
  }
}
