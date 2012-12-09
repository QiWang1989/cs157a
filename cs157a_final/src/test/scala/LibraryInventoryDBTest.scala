/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/5/12
 * Time: 9:46 PM
 * To change this template use File | Settings | File Templates.
 */
import org.scalatest.{Tag, FunSpec}
import org.scalatest.junit.JUnitRunner
import org.squeryl.PrimitiveTypeMode._

@org.junit.runner.RunWith(classOf[JUnitRunner])
class LibraryInventoryDBTest extends FunSpec{
  describe("LibraryInventoryDB ") {
    val libraryInventoryDB = new LibraryInventoryDB{}

    it("add(entry:T, copy:Int=1) should add books to the library"){
      libraryInventoryDB.add(new LibraryEntry("uniqueNo1","Life of Pi", "An Lee"),2)
      libraryInventoryDB.add(new LibraryEntry("uniqueNo1","Life of Pi", "An Lee"),3)

      libraryInventoryDB.executeQuery{
        val result = libraryInventoryDB.ifExist("uniqueNo1").get
        assert(result.copyInStock == 5)
        assert(result.totalCopy == 5)
      }
      libraryInventoryDB.delete("uniqueNo1")
    }

    it("addCopy(id: String, copy:Int) should add copy to existing book the library"){
      libraryInventoryDB.add(new LibraryEntry("uniqueNo7","Life of Pi", "An Lee"))
      libraryInventoryDB.addCopy("uniqueNo7",9)

      libraryInventoryDB.executeQuery{
        val result = libraryInventoryDB.ifExist("uniqueNo7").get
        assert(result.copyInStock == 10)
        assert(result.totalCopy == 10)
      }
      libraryInventoryDB.delete("uniqueNo7")
    }

    it("delete(id: String) should delete book entry from the library"){
      libraryInventoryDB.add(new LibraryEntry("uniqueNo1","Life of Pi", "An Lee"))
      libraryInventoryDB.delete("uniqueNo1")
      libraryInventoryDB.executeQuery{
        assert((from(Library.book)(bk => where(bk.id === "uniqueNo1") select (bk))).isEmpty)
      }
    }

    it("deleteCopy(id: String, copy:Int=1) should delete specified number of copy from the library"){
      libraryInventoryDB.add(new LibraryEntry("uniqueNo8","Life of Pi", "An Lee"), 8)
      libraryInventoryDB.deleteCopy("uniqueNo8",5)
      libraryInventoryDB.executeQuery{
        val result = libraryInventoryDB.ifExist("uniqueNo8").get
        assert(result.copyInStock == 3)
        assert(result.totalCopy == 3)
      }
      libraryInventoryDB.delete("uniqueNo8")
    }

    //test-only LibraryInventoryDBTest -- include(borrow)
    it("borrowBook(user: String, id: String) should delete one book entry from the library", Tag("borrow")){
      libraryInventoryDB.add(new LibraryEntry("uniqueNo2","Life of Pi", "An Lee"))
      libraryInventoryDB.borrowBook("uniqueNo2", "Qi")

      libraryInventoryDB.executeQuery{
        from(Library.book)(bk => where(bk.id === "uniqueNo2") select (bk.copyInStock)) foreach(_==0)
        assert(!(from(Library.borrow)(borrow => where(borrow.user === "Qi" and borrow.id === "uniqueNo2") select (borrow))).isEmpty)
        Library.borrow.deleteWhere(borrow => borrow.user === "Qi" and borrow.id === "uniqueNo2")
      }
      libraryInventoryDB.delete("uniqueNo2")
    }

    it("returnBook(user: String, id: String) should delete one copy from book entry record", Tag("return")){
      libraryInventoryDB.add(new LibraryEntry("uniqueNo3","Life of Pi", "An Lee"))
      libraryInventoryDB.borrowBook("uniqueNo3","Qi")
      libraryInventoryDB.returnBook("uniqueNo3","Qi")

      libraryInventoryDB.executeQuery{
        from(Library.book)(bk => where(bk.id === "uniqueNo3") select (bk.copyInStock)) foreach(copy=>assert(copy==1))
        assert((from(Library.borrow)(borrow => where(borrow.id === "uniqueNo3" and borrow.user === "Qi") select (borrow))).isEmpty)
      }
      libraryInventoryDB.delete("uniqueNo3")
    }
  }
}
