///**
// * Created with IntelliJ IDEA.
// * User: qiwan
// * Date: 11/10/12
// * Time: 10:51 AM
// * To change this template use File | Settings | File Templates.
// */
//import org.scalatest.FunSpec
//import org.scalatest.junit.JUnitRunner
//
//@org.junit.runner.RunWith(classOf[JUnitRunner])
//class CalculatorTest extends FunSpec{
//   describe("opMapping method on Calculator[Int] with parameter"){
//     val calc = new Calculator(x => IntWrapper(x))
//     calc.stack.push(IntWrapper(897))
//     calc.stack.push(IntWrapper(54))
//
//     it("'+' should pop two values off the stack, add them together, and push their sum onto the stack"){
//        calc.opMapping("+")
//        assert(calc.stack.pop() == IntWrapper(897+54))
//     }
//
//     it("'-' should subtract the number on the top of the stack from the one beneath it, " +
//       "popping both, and push the difference"){
//       calc.opMapping("-")
//       assert(calc.stack.pop() == IntWrapper(897-54))
//     }
//
//     it("'*' should pop two values off the stack, mulitply them together, and push their product onto the stack"){
//       calc.opMapping("*")
//       assert(calc.stack.pop() == IntWrapper(897*54))
//     }
//
//     it("'/' should pop two values off the stack, divide the one that was second from the top by the one that was " +
//       "on top, and push the quotient onto the stack"){
//       calc.opMapping("/")
//       assert(calc.stack.pop() == IntWrapper(897/54))
//     }
//
//     it("'%' should pop two values off the stack, divide the one that was second from the top by the one that was " +
//       "on top, and push the remainder onto the stack"){
//       calc.opMapping("*")
//       assert(calc.stack.pop() == IntWrapper(897%54))
//     }
//   }
//}
