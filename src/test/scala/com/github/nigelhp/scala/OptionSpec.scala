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

    it("can be used in a for comprehension") {
      case class Value(optionalValue: Option[Int])

      val values: List[Value] = List(Value(Some(42)), Value(None), Value(Some(666)))
      val x = for {
        v <- values
        i <- v.optionalValue
      } yield i

      assert(x === List(42, 666))
    }

    it("can be chained with orElse") {
      assert(None.orElse(Some(42)) === Some(42))
      assert(Some(42).orElse(Some(666)) === Some(42))
    }

    describe("None") {
      it("can be created from a null via a factory method") {
        assert(Option(null) === None)
      }

      it("is not defined") {
        assert(!None.isDefined)
      }

      it("is empty") {
        assert(None.isEmpty)
      }

      it("does not have a value that can be accessed") {
        intercept[NoSuchElementException] {
          None.get
        }
      }

      it("and so will always return the default value when specified") {
        assert(None.getOrElse(666) === 666)
      }

      // verbose and non-idiomatic
      it("can be pattern matched") {
        val option: Option[Int] = None
        val n: Int = option match {
          case None => 0
          case Some(x) => x
        }

        assert(n === 0)
      }

      it("will not invoke the supplied side-effecting function when foreach is called") {
        var count = 0
        None.foreach(_ => count += 1)

        assert(count === 0)
      }

      it("will always map to another None") {
        val sourceOption: Option[Int] = None
        val targetOption: Option[String] = sourceOption.map(n => n.toString)

        assert(!targetOption.isDefined)
      }

      it("will always flatmap to another None") {
        val sourceOption: Option[Int] = None
        val targetOption: Option[String] = sourceOption.flatMap(n => Some(n.toString))

        assert(!targetOption.isDefined)
      }

      it("will always return None from filter") {
        val sourceOption: Option[Int] = None

        assert(sourceOption.filter(n => true) === None)
        assert(sourceOption.filter(n => false) === None)
      }
    }

    describe("Some") {
      it("can be created from a value via a factory method") {
        assert(Option(42) === Some(42))
      }

      it("is defined") {
        assert(Some(42).isDefined)
      }

      it("is not empty") {
        assert(Some(42).nonEmpty)
      }

      it("has a value that can be accessed") {
        assert(Some(42).get === 42)
      }

      it("and so will always return that value rather than the default when specified") {
        assert(Some(42).getOrElse(666) === 42)
      }

      // verbose and non-idiomatic
      it("can be pattern matched") {
        val option: Option[Int] = Some(42)
        val n: Int = option match {
          case None => 0
          case Some(x) => x
        }

        assert(n === 42)
      }

      it("will invoke the supplied side-effecting function once when foreach is called") {
        var count = 0
        Some(42).foreach(_ => count += 1)

        assert(count === 1)
      }

      it("will map to another Some") {
        val sourceOption: Option[Int] = Some(42)
        val targetOption: Option[String] = sourceOption.map(n => n.toString)

        assert(targetOption.isDefined)
      }

      it("will return a nested Option if the mapping function supplied to map itself returns an Option") {
        assert(Some(42).map(Option(_)) === Some(Some(42)))
        assert(Some(42).map(n => None) === Some(None))
      }

      it("will return the mapped Option if the mapping function supplied to flatmap returns an Option") {
        assert(Some(42).flatMap(Option(_)) === Some(42))
        assert(Some(42).flatMap(n => None) === None)
      }

      it("will return Some if a filter predicate is satisfied") {
        assert(Some(42).filter(n => true) === Some(42))
      }

      it("will return None if a filter predicate is not satisfied") {
        assert(Some(42).filter(n => false) === None)
      }
    }
  }
}
