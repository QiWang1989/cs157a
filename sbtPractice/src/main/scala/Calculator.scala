/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 11/7/12
 * Time: 8:47 PM
 * This calculator supports both PN, Int, Fraction[PN], Fraction[Int].
 * {{{
 *   Example:
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
 * This calculator also supports input from a file (file name must contains ".txt")
 * {{{
 *   Example:
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
  //   val param = args.toList.map(_.toLowerCase)
  //   param match {
  //    // initialize PN calculator
  //    case "-m"::"pn"::Nil => new Calculator(x => ImplicitConversion.intToPN(x(0)))
  //    // initialize Int calculator
  //    case "-m"::"int"::Nil => new Calculator(x => IntWrapper(x(0)))
  //    // initialize Fraction[PN] calculator
  //8/8    case "-m"::"fraction[pn]"::Nil => new Calculator(x => Fraction(ImplicitConversion.intToPN(x(0)), ImplicitConversion.intToPN(x(1))))
  //    // initialize Fraction[Int] calculator
  //    case "-m"::"fraction[int]"::Nil => new Calculator(x => Fraction(IntWrapper(x(0)), IntWrapper(x(1))))
  //    case _ => "wrong arguments"
  //  }
  def mulTable(n: Int) = {
    val result = 1 to n map {
      i =>
        (1 to n map (x => (i * x))) mkString (" \t")
    }
    result.mkString("\n")
  } //> mulTable: (n: Int)String
  print(mulTable(12))
}

class Calculator[T <: Arithmetic[T]](val fac: List[Int] => T) {

  val stack = mutable.Stack[T]()

  console {
    case Quit =>
      true
    case Line(s) =>
      val ls = s.split(" ").toList
      ls.foreach(commandMapping orElse opMapping orElse numMapping)
      false
    case File(filename) =>
      for (line <- Source.fromFile(filename).getLines()) {
        val ls = line.split(" ").toList
        ls.foreach(opMapping orElse numMapping)
      }
      false
    case _ =>
      false
  }

  def console(handler: JLineEvent => Boolean) {
    val consoleReader = new jline.console.ConsoleReader()
    var finished = false

    while (!finished) {
      consoleReader.addCompleter(new jline.console.completer.FileNameCompleter)
      consoleReader.setPrompt("Qi's Calc> ")
      val line = consoleReader.readLine()
      finished = line match {
        case ("q" | null) => handler(Quit)
        case ln if ln.contains(".txt") => handler(File(ln))
        case ln: String => handler(Line(ln))
      }
    }
  }

  def commandMapping: PartialFunction[String, Unit] = {
    case "p" =>
      if (!stack.isEmpty)
        println(stack.pop)
      false
  }

  def opMapping: PartialFunction[String, Unit] = {
    case "+" =>
      stack.push(stack.pop + stack.pop)
    case "-" =>
      val (top, second) = (stack.pop, stack.pop)
      stack.push(second - top)
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
    case x: String if x.contains("/") =>
      val ls = x.split("/").toList map (_.toInt)
      stack.push(fac(ls))
    case x: String => stack.push(fac(List(x.toInt)))
  }
}