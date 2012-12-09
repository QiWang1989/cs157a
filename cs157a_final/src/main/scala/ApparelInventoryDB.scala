import org.squeryl.PrimitiveTypeMode._
import scala.Some

/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 12:00 PM
 * To change this template use File | Settings | File Templates.
 */
case class ApparelEntry(override val id:String, style:String, location:String, override val copyInStock: Int=1, override val totalCopy: Int=1)
  extends Entry(id, copyInStock, totalCopy){
  def setCopy(copy:Int):ApparelEntry = ApparelEntry(this.id, this.style, this.location, copy, copy)
  override def toString = id+"\t"+style+"\t"+location+"\t"+copyInStock+"\t"+totalCopy
}

object Apparel extends org.squeryl.Schema {
  val apparel = table[ApparelEntry]("apparel")
}

trait ApparelInventoryDB extends InventoryDB{
  type  T = ApparelEntry
  def mainTable = Apparel.apparel
  def database="apparel"
}
