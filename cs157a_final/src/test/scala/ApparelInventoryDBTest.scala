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
class ApparelInventoryDBTest extends FunSpec {
  describe("ApparelInventoryDBTest ") {
    val apparelInventoryDB = new ApparelInventoryDB {}

    it("add(entry:T, copy:Int=1) should add books to the library") {
      apparelInventoryDB.add(new ApparelEntry("uniqueNo1", "Jean", "Waltham"), 2)
      apparelInventoryDB.add(new ApparelEntry("uniqueNo1", "Jean", "Waltham"), 3)

      apparelInventoryDB.executeQuery{
        val result = apparelInventoryDB.ifExist("uniqueNo1").get
        assert(result.copyInStock == 5)
        assert(result.totalCopy == 5)
      }
      apparelInventoryDB.delete("uniqueNo1")
    }

    it("addCopy(id: String, copy:Int) should add copy to existing book the library") {
      apparelInventoryDB.add(new ApparelEntry("uniqueNo7", "Jean", "Waltham"))
      apparelInventoryDB.addCopy("uniqueNo7", 9)

      apparelInventoryDB.executeQuery{
        val result = apparelInventoryDB.ifExist("uniqueNo7").get
        assert(result.copyInStock == 10)
        assert(result.totalCopy == 10)
      }
      apparelInventoryDB.delete("uniqueNo7")
    }

    it("delete(id: String) should delete book entry from the library") {
      apparelInventoryDB.add(new ApparelEntry("uniqueNo2", "Jean", "Waltham"))
      apparelInventoryDB.delete("uniqueNo2")
      apparelInventoryDB.executeQuery{
        assert((from(Apparel.apparel)(ap => where(ap.id === "uniqueNo2") select (ap))).isEmpty)
      }
    }

    it("deleteCopy(id: String, copy:Int=1) should delete specified number of copy from the library") {
      apparelInventoryDB.add(new ApparelEntry("uniqueNo8", "Jean", "Waltham"), 8)
      apparelInventoryDB.deleteCopy("uniqueNo8", 5)
      apparelInventoryDB.executeQuery{
        val result = apparelInventoryDB.ifExist("uniqueNo8").get
        assert(result.copyInStock == 3)
        assert(result.totalCopy == 3)
      }
      apparelInventoryDB.delete("uniqueNo8")
    }
  }
}
