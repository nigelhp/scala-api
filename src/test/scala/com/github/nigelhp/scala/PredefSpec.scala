package com.github.nigelhp.scala

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class PredefSpec extends FunSpec {

  describe("??? can be used as a stub implementation when defining a method") {
    def someMethod(n: Int): Unit = ???

    it("throws a NotImplementedError if the method is invoked") {
      intercept[NotImplementedError] {
        someMethod(42)
      }
    }
  }

  /*
   * Note that scalatest provides its own implementation of assert,
   * and so we must explicitly reference the standard Predef implementation.
   */
  describe("assert") {
    it("throws an AssertionError if the supplied expression is false") {
      the [AssertionError] thrownBy {
        Predef.assert(1 == 2, "some message")
      } should have message "assertion failed: some message"
    }
  }

  describe("assume") {
    it("throws an AssertionError if the specified assumption does not hold") {
      the [AssertionError] thrownBy {
        Predef.assume(1 == 2)
      } should have message "assumption failed"
    }
  }

}
