/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 11:57 AM
 * To change this template use File | Settings | File Templates.
 */


import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{SessionFactory, Schema, Session}
import org.squeryl.annotations.Column
import java.util.Date
import java.sql.Timestamp
import scala.Some

abstract class Entry

trait InventoryDB {
  type T <: Entry

  def executeQuery[T](db:String)(p: =>T):T = {
    Class.forName("com.mysql.jdbc.Driver")

    SessionFactory.concreteFactory = Some(()=>
      Session.create(
        java.sql.DriverManager.getConnection("jdbc:mysql://localhost/"+db,"root","110110Wq?"),new org.squeryl.adapters.MySQLAdapter))

    inTransaction {
       p
    }
  }
  // add an initial copy or one additional copy
  // increase both copyInStock and totalCopy
  def add(entry:T)

  // remove all copies of this book
  def delete(id:String)

  // add one additional copy
  // increase both copyInStock and totalCopy
  def addOneCopy(id:String)

  //delete one copy from library
  //decrease both copyInStock and totalCopy
  def deleteOneCopy(id:String)
}
