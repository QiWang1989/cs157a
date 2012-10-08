/*
 * Author: Qi Wang
 * Assignment1
 */


import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner

@org.junit.runner.RunWith(classOf[JUnitRunner])
class IntOperationTest extends FunSpec {
  import  ImplicitConversion._

  describe("Integer add function") {
    it("should add two lists with arbitraty length correctly for base 2") {
      val num1 = List(1,0,0,1,1,0,1,0,0,1,0)
      val num2 = List(1,0,1,1,1)
      assert(num1 + (num2, 2) equals PN(List(1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1)))
      assert(num2 + (num1, 2) equals PN(List(1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1)))
    }

    it("should add two lists with arbitraty length correctly for base 8") {
      val num1 = List(2,3,2,2)
      val num2 = List(2,7)
      val result = num1+(num2,8)
      assert(num1 + (num2, 8) equals PN(List(2, 3, 5, 1)))
      assert(num2 + (num1, 8) equals PN(List(2, 3, 5, 1)))
    }

    it("should add two lists with arbitraty length correctly for base 10") {
      val num1 = List(1,2,3,4)
      val num2 = List(2,3)
      assert(num1 + num2 equals PN( List(1, 2, 5, 7)))
      assert(num2 + num1 equals PN( List(1, 2, 5, 7)))
    }

    it("should add two lists with arbitraty length correctly for base 16") {
      val num1 = List(4, 13, 2)
      val num2 = List(1, 7)
      assert(num1 + (num2, 16) equals PN( List(4, 14, 9)))
      assert(num2 + (num1, 16) equals PN( List(4, 14, 9)))
    }

    it("should add two lists with arbitraty length correctly for mixed base (24, 60)") {
      val num1 = List(4, 16, 2)
      val num2 = List(9,59)
      assert(num1 + (num2, List(24, 60)) equals PN( List(5, 2, 1)))
      assert(num2 + (num1, List(24, 60)) equals PN( List(5, 2, 1)))
    }
  }

  describe("Integer subtract function") {
    it("should subtract two lists with arbitraty length correctly for base 2") {
      val num1 = PN(List(1,0,0,1,1,0,1,0,0,1,0))
      val num2 = List(1,0,1,1,1)
      val result = List(1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1)
      assert((num1 - (num2, 2))._1 equals PN( result))
      assert(num2 + (result, 2) equals PN( num1))
    }

    it("should subtract two lists with arbitraty length correctly for base 8") {
      val num1 = PN(List(2, 3, 2, 2))
      val num2 = List(2, 7, 9)
      val result = List(2, 0, 2, 1)
      assert((num1 - (num2, 8))._1 equals PN( result))
      assert(num2 + (result, 8) equals PN( num1))
    }

    it("should subtract two lists with arbitraty length correctly for base 10") {
      val num1 = PN(List(1, 2, 3, 4))
      val num2 = List(9, 8)
      val result = List(1, 1, 3, 6)
      assert((num1 - num2)._1 equals PN( List(1, 1, 3, 6)))
      assert((result + num2) equals PN( num1))
    }

    it("should subtract two lists with arbitraty length correctly for base 16") {
      val num1 = PN(List(4, 13, 2))
      val num2 = List(1, 7)
      val result = List(4, 11, 11)
      assert((num1 - (num2, 16))._1 equals PN( result))
      assert((num2 + (result, 16)) equals PN( num1))
    }

    it("should add two lists with arbitraty length correctly for mixed base (24, 60)") {
      val num1 = PN(List(4, 16, 2))
      val num2 = List(19,59)
      val result = List(3, 20, 3)
      assert((num1 - (num2, List(24, 60)))._1 equals PN( result))
      assert((num1 - (num2, List(24,60)))._2 )
      assert((num2 + (result, List(24, 60))) equals PN( num1))
    }

    it("if result is negative, it should be represented in b's complement"){
      val num1 = PN(List(1,8,5))
      val num2 = List(3,2,9)
      assert((num1 - num2)._1 equals PN( List(8,5,6)))
      assert(!(num1 - num2)._2 )
    }
  }

  describe("Integer mul function") {
    it("should multiply two lists with arbitraty length correctly for base 2") {
      val num1 = List(1, 1, 0, 1)
      val num2 = List(1, 0, 1)
      assert((num1 * (num2, 2) equals PN( List(1, 0, 0, 0, 0, 0, 1))))
      assert((num2 * (num1, 2) equals PN(List(1, 0, 0, 0, 0, 0, 1))))
    }
    
    it("should multiply two lists with arbitraty length correctly for base 8") {
      val num1 = List(2, 3, 2, 2)
      val num2 = List(2, 7)
      assert(num1 * (num2, 8) equals PN( List(6, 7, 3, 3, 6)))
      assert(num2 * (num1, 8) equals PN( List(6, 7, 3, 3, 6)) )
    }
    
    it("should multiply two lists with arbitraty length correctly for base 10") {
      val num1 = List(1, 2, 3, 4)
      val num2 = List(2, 3)
      assert((num1 * num2) equals PN( List(2, 8, 3, 8, 2)))
      assert((num2 * num1) equals PN( List(2, 8, 3, 8, 2)))
    }
    
    it("should multiply two lists with arbitraty length correctly for base 16") {
      val num1 = List(4, 13, 2)
      val num2 = List(1, 7)
      assert(num1 * (num2, 16) equals PN( List(6, 14, 13, 14)) )
      assert(num2 * (num1, 16) equals PN( List(6, 14, 13, 14)))
    }
  }


  describe("Integer longDiv function"){
    it("should divide two lists for base 2"){
      val num1 = List(1, 1, 1, 0, 1)
      val num2 = List(1, 0, 1)
      assert((num1 / (num2,2))._1 equals  PN(List(1,0,1)))
      assert((num1 / (num2,2))._2 equals PN( List(1,0,0)))
      assert(((num2 * (List(1,0,1),2)) + (List(1,0,0),2))equals PN( List(1,1,1,0,1)))
    }

    it("should divide two lists for base 10 when v is only one digit"){
      val num1 = List(7, 8, 9)
      val num2 = List(9)
      assert((num1 / (num2,10))._1 equals  PN( List(8, 7)))
      assert((num1 / (num2,10))._2 equals PN( List(6)))
      assert(((num2 * List(8,7)) + List(6)) equals PN( List(7,8,9)))
    }

    it("should divide two lists for base 10"){
      val num1 = List(7, 8, 9)
      val num2 = List(9, 8)
      assert((num1 / (num2, 10))._1 equals PN( List(8)) )
      assert((num1 / (num2, 10))._2 equals PN( List(5)))
      assert(((num2 * (List(8), 10)) + List(5)) equals PN( List(7,8,9)))
    }
  }
}