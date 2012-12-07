/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 12:00 PM
 * To change this template use File | Settings | File Templates.
 */
case class ApparelEntry(id:String, model:String, location:String) extends Entry

trait ApparelInventoryDB extends InventoryDB{
  type  T = ApparelEntry
  override def add(entry:T) = {
    println("add an entry to Apparel Inventory Table")
  }
  override def delete(id:String) = {
    println("delete an entry to Apparel Inventory Table")
  }
}
