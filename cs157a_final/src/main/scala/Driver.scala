/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 12/2/12
 * Time: 11:56 AM
 * To change this template use File | Settings | File Templates.
 */
object Driver extends App{
  println("Choose inventory system:")
  println("1. Library")
  println("2. Apparel")

  val choice = readLine()
  choice.toLowerCase match {
    case "library" =>
      val inventory = new LibraryInventory
      inventory.cmdLoop
    case "apparel" =>
      val inventory = new ApparelInventory
      inventory.adminAddEntry(ApparelEntry("NO1","Shorts", "A&F"))
  }
}
