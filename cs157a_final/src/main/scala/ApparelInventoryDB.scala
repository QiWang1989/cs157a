/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 12:00 PM
 * To change this template use File | Settings | File Templates.
 */
case class ApparelEntry(id:String, model:String, location:String) extends Entry{
  override def toString = id+"\t"+model+"\t"+location
}

trait ApparelInventoryDB extends InventoryDB{
  type  T = ApparelEntry

  // add an initial copy or one additional copy
  // increase both copyInStock and totalCopy
  def add(entry:T) = {

  }

  // remove all copies of this book
  def delete(id:String) = {

  }

  // add one additional copy
  // increase both copyInStock and totalCopy
  def addOneCopy(id:String) = {

  }

  //delete one copy from library
  //decrease both copyInStock and totalCopy
  def deleteOneCopy(id:String) = {

  }

  def list = {
  }
}
