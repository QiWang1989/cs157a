/**
 * Created with IntelliJ IDEA.
 * User: qiwang
 * Date: 9/26/12
 * Time: 10:10 AM
 * To change this template use File | Settings | File Templates.
 */
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Tag

@org.junit.runner.RunWith(classOf[JUnitRunner])
class FractionTest extends FunSpec{
  describe("Fraction[PN]:") {
    import  ImplicitConversion.intToList

    it("should add two fractions of type PN and simplify the result"){
      assert(Fraction(PN(3),PN(2))+Fraction(PN(4),PN(6)) equals Fraction(PN(13),PN(6)))
    }

    it("should subtract two fractions of type PN and simplify the result"){
      assert(Fraction(PN(3),PN(2))-Fraction(PN(4),PN(6)) equals  Fraction(PN(5),PN(6)))
    }

    it("should multiply two fractions of type PN and simplify the result"){
      assert(Fraction(PN(3),PN(2))*Fraction(PN(4),PN(6)) equals Fraction(PN(1),PN(1)))
    }

    it("should divide two fractions of type PN and simplify the result"){
      assert(Fraction(PN(3),PN(2))/Fraction(PN(4),PN(6)) equals Fraction(PN(9),PN(4)))
    }
  }

  describe("Fraction[Int]:") {
    it("should add big two fractions of type Int and simplify the result", Tag("IntBigAdd")){
      assert(Fraction(IntWrapper(123), IntWrapper(456)) + Fraction(IntWrapper(789),
        IntWrapper(987)) equals Fraction(IntWrapper(53465), IntWrapper(50008)))
    }

    it("should add two fractions of type Int and simplify the result", Tag("IntAdd")){
      assert(Fraction(IntWrapper(3), IntWrapper(2)) + Fraction(IntWrapper(4),
        IntWrapper(6)) equals Fraction(IntWrapper(13), IntWrapper(6)))
    }

    it("should subtract two fractions of type Int and simplify the result"){
      assert(Fraction(IntWrapper(3),IntWrapper(2))-
        Fraction(IntWrapper(4),IntWrapper(6)) equals  Fraction(IntWrapper(5),IntWrapper(6)))
    }

    it("should multiply two fractions of type Int and simplify the result"){
      assert(Fraction(IntWrapper(3),IntWrapper(2))*
        Fraction(IntWrapper(4),IntWrapper(6)) equals Fraction(IntWrapper(1),IntWrapper(1)))
    }

    it("should divide two fractions of type Int and simplify the result"){
      assert(Fraction(IntWrapper(3),IntWrapper(2))/
        Fraction(IntWrapper(4),IntWrapper(6)) equals Fraction(IntWrapper(9),IntWrapper(4)))
    }
  }
}
