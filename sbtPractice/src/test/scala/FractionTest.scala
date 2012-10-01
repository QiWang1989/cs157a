/**
 * Created with IntelliJ IDEA.
 * User: qiwang
 * Date: 9/26/12
 * Time: 10:10 AM
 * To change this template use File | Settings | File Templates.
 */
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner

@org.junit.runner.RunWith(classOf[JUnitRunner])
class FractionTest extends FunSpec{
  import  ImplicitConversion._

  describe("Fraction + Fraction: ") {
    it("should add two fractions and simplify the result"){
      assert(Fraction(PN(3),PN(2))+Fraction(PN(4),PN(6)) equals Fraction(13,6))
    }
  }

  describe("Fraction - Fraction: ") {
    it("should subtract two fractions and simplify the result"){
      assert(Fraction(PN(3),PN(2))-Fraction(PN(4),PN(6)) equals  Fraction(PN(5),PN(6)))
    }
  }

  describe("Fraction * Fraction: ") {
    it("should multiply two fractions and simplify the result"){
      assert(Fraction(PN(3),PN(2))*Fraction(PN(4),PN(6)) equals Fraction(PN(1),PN(1)))
    }
  }

  describe("Fraction / Fraction: ") {
    it("should divide two fractions and simplify the result"){
      assert(Fraction(PN(3),PN(2))/Fraction(PN(4),PN(6)) equals Fraction(PN(9),PN(4)))
    }
  }
}
