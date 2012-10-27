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
  import  ImplicitConversion.intToList

  describe("Fraction[PN]:") {
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
      assert(Fraction(123, 456) + Fraction(789, 987) equals Fraction(53465, 50008))
    }

    it("should add two fractions of type Int and simplify the result", Tag("IntAdd")){
      assert(Fraction(3, 2) + Fraction(4, 6) equals Fraction(13, 6))
    }

    it("should subtract two fractions of type Int and simplify the result"){
      assert(Fraction(3,2)-Fraction(4,6) equals  Fraction(5,6))
    }

    it("should multiply two fractions of type Int and simplify the result"){
      assert(Fraction(3,2)*Fraction(4,6) equals Fraction(1,1))
    }

    it("should divide two fractions of type Int and simplify the result"){
      assert(Fraction(3,2)/Fraction(4,6) equals Fraction(9,4))
    }
  }
}
