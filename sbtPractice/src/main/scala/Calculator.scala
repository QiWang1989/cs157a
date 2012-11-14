/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 11/7/12
 * Time: 8:47 PM
 * This calculator supports both PN and Int.
 * {{{
 *   Example:
 *     Choose number system: pn
 *     Qi's Calc> 80 70 -
 *     80 70 -
 *     Qi's Calc> 67 +
 *     67 +
 *     Qi's Calc> p
 *     p
 *     List(7, 7)
 *     Qi's Calc>
 * }}}
 *
 * This calculator also supports input from a file
 * {{{
 *   Example:
 *     Choose number system: Int
 *     Qi's Calc> /tmp/cs157a.txt
 *     /tmp/cs157a.txt
 *     Qi's Calc> 90 +
 *     Qi's Calc> p
 *     p
 *     220
 * }}}
 */

import collection.mutable
import io.Source

sealed trait JLineEvent
case class Line(value: String) extends JLineEvent
case class File(filename: String) extends JLineEvent
case object Quit extends JLineEvent
case object Print extends JLineEvent

object Calculator extends App {
  val numSys = readLine("Choose number system: ")
  numSys.toLowerCase match {
    case "pn" => new Calculator(x => ImplicitConversion.intToPN(x))
    case "int" => new Calculator(x => IntWrapper(x))
  }
}

class Calculator[T <: Arithmetic[T]](val fac: Int => T) {
  val stack = mutable.Stack[T]()

  console {
    case Quit =>
      true
    case Line(s) =>
      val ls = s.split(" ").toList
      ls.foreach (opMapping orElse numMapping)
      false
    case Print =>
      if (!stack.isEmpty)
        println(stack.pop)
      false
    case File(filename) =>
      for (line <- Source.fromFile(filename).getLines()) {
        val ls = line.split(" ").toList
        ls.foreach (opMapping orElse numMapping)
      }
      false
    case _ =>
      false
  }

  def console(handler: JLineEvent => Boolean) {
    val consoleReader = new jline.console.ConsoleReader()
//    consoleReader.setKeyMap(KeyMap.VI_INSERT)
    var finished = false

    while (!finished) {
      consoleReader.addCompleter(new jline.console.completer.FileNameCompleter)
      consoleReader.setPrompt("Qi's Calc> ")
      val line = consoleReader.readLine()
      finished = line match {
        case ("q" | null) => handler(Quit)
        case "p" => handler(Print)
        case ln if ln.contains(".txt") => handler(File(ln))
        case ln: String => handler(Line(ln))
      }
    }
  }

  def opMapping: PartialFunction[String, Unit] = {
    case "+" =>
      stack.push(stack.pop + stack.pop)
    case "-" =>
      val (top, second) = (stack.pop, stack.pop)
      stack.push( second - top)
    case "*" =>
      stack.push(stack.pop * stack.pop)
    case "/" =>
      val (top, second) = (stack.pop, stack.pop)
      stack.push(second / top)
    case "%" =>
      val (top, second) = (stack.pop, stack.pop)
      stack.push(second % top)
  }

  def numMapping: PartialFunction[String, Unit] = {
    case x:String => stack.push(fac(x.toInt))
  }
}