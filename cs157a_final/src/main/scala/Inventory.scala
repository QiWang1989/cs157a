/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 11:56 AM
 * To change this template use File | Settings | File Templates.
 */
trait Inventory {
  self:InventoryDB =>

  def adminAddEntry(entry: T) ={
    add(entry)
  }

  // remove all copies of this book
  def adminDeleteEntry(id: String) ={
    delete(id)
  }
}
