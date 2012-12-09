/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 11:57 AM
 * To change this template use File | Settings | File Templates.
 */


import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Table, SessionFactory, Schema, Session}
import org.squeryl.annotations.Column
import java.util.Date
import java.sql.Timestamp
import scala.Some

abstract class Entry(val id:String, val copyInStock:Int, val totalCopy:Int)

trait InventoryDB {
  type T <: Entry{
    def setCopy(copy:Int):T
  }

  def mainTable:Table[T]
  def database:String

  def executeQuery[T](p: =>T):T = {
    Class.forName("com.mysql.jdbc.Driver")

    SessionFactory.concreteFactory = Some(()=>
      Session.create(
        java.sql.DriverManager.getConnection("jdbc:mysql://localhost/"+database,"root","110110Wq?"),new org.squeryl.adapters.MySQLAdapter))

    inTransaction {
       p
    }
  }
  // add an initial copy or one additional copy
  // increase both copyInStock and totalCopy
  def add(entry:T, copy:Int=1) = executeQuery{
    ifExist(entry.id) match {
      case Some(x) =>
        addCopyHelper(entry.id,copy)
      case None =>
        val e = if (copy == 1) entry else entry.setCopy(copy)
    }
  }

  // add one additional copy
  // increase both copyInStock and totalCopy
  def addCopy(id:String, copy:Int) = executeQuery{
    addCopyHelper(id, copy)
  }

  def addCopyHelper(id: String, copy:Int) = {
    update(mainTable)(x =>
      where(x.id === id)
        set(x.copyInStock := x.copyInStock.~ + copy,
        x.totalCopy := x.totalCopy.~ + copy)
    )
  }

  // remove all copies of this book
  def delete(id:String) = executeQuery{
    mainTable.deleteWhere(x => x.id === id) match {
      case 1 => println("Entry deleted successfully.")
      case 0 => println("Entry doesn't exist.")
      case _ => println("Error.")
    }
  }

  //delete one copy from library
  //decrease both copyInStock and totalCopy
  def deleteCopy(id:String, copy:Int=1) = executeQuery{
    ifExist(id) match {
      case Some(x) if x.copyInStock >= copy =>
        update(mainTable)(i =>
          where(i.id === id)
            set(i.copyInStock := i.copyInStock.~ - copy,
            i.totalCopy := i.totalCopy.~ - copy)
        )
        println("Delete one copy of <" + id + ">")
      case Some(ap) =>
        println("Out of stock")
      case None =>
        println("Doesn't have this style.")
    }
  }

  def list(id:String) = executeQuery{
    val result = from(mainTable)(x=>select(x))

    if (id == "all"){
      result.isEmpty match{
        case false =>
          println("====================================================")
          result.foreach(println(_))
          println("====================================================")
        case true => println("Empty inventory")
      }
    }else{
      ifExist(id) match{
        case Some(entry) =>
          println("====================================================")
          println(entry)
          println("====================================================")
        case None => println("id doesn't exist")
      }
    }
  }

  def ifExist(id: String):Option[T] = {
    var result: org.squeryl.Query[T] = null
    inTransaction {
      result = from(mainTable)(x => where(x.id === id) select (x))
    }

    result.isEmpty match {
      case false => Some(result.head)
      case true => None
    }
  }
}
