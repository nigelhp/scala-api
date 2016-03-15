package com.github.nigelhp.scala

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class OptionSpec extends FunSpec {

  describe("an Optional value") {
    it("can be None") {
      val none: Option[Int] = None
      none shouldBe a [Option[_]]
    }

    it("can be Some(value)") {
      val some: Option[Int] = Some(42)
      some shouldBe a [Option[_]]
    }

    describe("None") {
      it("can be created from a null via the factory method") {
        assert (Option(null) === None)
      }

      it("is not defined") {
        assert (!None.isDefined)
      }

      it("does not have a value that can be accessed") {
        intercept[NoSuchElementException] {
          None.get
        }
      }

      it("and so will always return the default value when specified") {
        assert (None.getOrElse(666) === 666)
      }
    }

    describe("Some") {
      it("can be created from a value via the factory method") {
        assert (Option(42) === Some(42))
      }

      it("is defined") {
        assert (Some(42).isDefined)
      }

      it("has a value that can be accessed") {
        assert (Some(42).get === 42)
      }

      it("and so will always return that value rather than the default when specified") {
        assert (Some(42).getOrElse(666) === 42)
      }
    }
  }
}
