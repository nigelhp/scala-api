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

      it("can be created from the empty factory method") {
        val option = Option.empty

        assert(option === None)
      }

      it("is not defined") {
        assert(!None.isDefined)
      }

      it("is empty") {
        assert(None.isEmpty)
        assert(!None.nonEmpty)
      }

      it("has a size of 0") {
        assert(None.size === 0)
      }

      it("does not contain anything") {
        assert(!None.contains(42))
      }

      it("does not have a value that can be accessed") {
        intercept[NoSuchElementException] {
          None.get
        }
      }

      it("and so will always return the default value when specified") {
        assert(None.getOrElse(666) === 666)
      }

      it("will always return false when exists is invoked") {
        val option: Option[Int] = None

        assert(!option.exists(_ => true))
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

      // fold is equivalent to scala.Option map f getOrElse ifEmpty
      it("will return the default value from fold") {
        val option: Option[Int] = None

        assert(option.fold(666) {i => i * i} === 666)
      }

      it("will always return true from forall") {
        assert(None.forall(_ => true))
        assert(None.forall(_ => false))
      }

      it("can be converted to a null value via orNull") {
        val option: Option[String] = None

        assert(option.orNull === null)
      }

      it("will always return the supplied right from toLeft") {
        assert(None.toLeft(42) === Right(42))
      }

      it("will always return the supplied left from toRight") {
        assert(None.toRight(42) === Left(42))
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
        val some = Some(42)

        assert(some.nonEmpty)
        assert(!some.isEmpty)
      }

      it("has a size of 1") {
        assert(Some(42).size === 1)
      }

      it("contains a value") {
        assert(Some(42).contains(42))
        assert(!Some(42).contains(666))
      }

      it("has a value that can be accessed") {
        assert(Some(42).get === 42)
      }

      it("and so will always return that value rather than the default when specified") {
        assert(Some(42).getOrElse(666) === 42)
      }

      it("can also return its value via orNull") {
        val value = "Hello World!"
        val option: Option[String] = Some(value)

        assert(option.orNull === value)
      }

      it("will return true if an exists predicate is satisfied") {
        val option: Option[Int] = Some(42)

        assert(option.exists(i => i % 2 == 0))
      }

      it("will return false if an exists predicate is not satisfied") {
        val option: Option[Int] = Some(42)

        assert(!option.exists(i => i % 2 == 1))
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

      // fold is equivalent to scala.Option map f getOrElse ifEmpty
      it("will return the mapped value from fold") {
        val option: Option[Int] = Some(42)

        assert(option.fold(666) {i => i * i} === 1764)
      }

      it("will return the result of the predicate from forall") {
        val option: Option[Int] = Some(42)

        assert(option.forall(_ => true))
        assert(!option.forall(_ => false))
      }

      it("will return its value as a left from toLeft") {
        assert(Some(42).toLeft(666) === Left(42))
      }

      it("will return its value as a right from toRight") {
        assert(Some(42).toRight(666) === Right(42))
      }
    }
  }
}
