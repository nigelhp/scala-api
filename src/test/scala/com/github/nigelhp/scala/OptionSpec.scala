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

    it("can be used in a for comprehension, which will eliminate Nones") {
      case class Value(optionalValue: Option[Int])

      val values: List[Value] = List(Value(Some(42)), Value(None), Value(Some(666)))
      val x = for {
        v <- values
        i <- v.optionalValue
      } yield i

      assert(x === List(42, 666))
    }

    describe("None") {
      it("can be created from a null by a factory method") {
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
      }

      it("is not nonEmpty") {
        assert(!None.nonEmpty)
      }

      it("has a size of 0") {
        assert(None.size === 0)
      }

      it("has a count of 0") {
        assert(None.count(_ => true) === 0)
      }

      describe("contains") {
        it("returns false") {
          assert(!None.contains(42))
          assert(!None.contains(None))
        }
      }

      describe("get") {
        it("throws an exception") {
          intercept[NoSuchElementException] {
            None.get
          }
        }
      }

      describe("getOrElse") {
        it("returns the supplied default") {
          assert(None.getOrElse(666) === 666)
        }
      }

      describe("exists") {
        it("returns false") {
          assert(!None.exists(_ => true))
        }
      }

      describe("find") {
        it("returns None") {
          assert(None.find(_ => true) === None)
        }
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

      describe("foreach") {
        it("will not inboke the supplied side-effecting function") {
          var count = 0

          None.foreach(_ => count += 1)

          assert(count === 0)
        }
      }

      describe("map") {
        it("returns None") {
          assert(None.map(_ => 42) === None)
        }
      }

      describe("filter") {
        it("returns None") {
          assert(None.filter(_ => true) === None)
        }
      }

      describe("filterNot") {
        it("returns None") {
          assert(None.filterNot(_ => false) === None)
        }
      }

      describe("flatMap") {
        it("returns None") {
          assert(None.flatMap(_ => Some(42)) === None)
        }
      }

      describe("fold") {
        // fold is equivalent to scala.Option map f getOrElse ifEmpty
        it("returns the supplied default value") {
          val option: Option[Int] = None

          assert(option.fold(666) {i => i * i} === 666)
        }
      }

      describe("forall") {
        it("returns true") {
          assert(None.forall(_ => true))
          assert(None.forall(_ => false))
        }
      }

      describe("toLeft") {
        it("returns the supplied argument as a Right") {
          assert(None.toLeft(42) === Right(42))
        }
      }

      it("will always return the supplied left from toRight") {
        assert(None.toRight(42) === Left(42))
      }

      describe("orElse") {
        it("returns the else") {
          assert(None.orElse(None) === None)
          assert(None.orElse(Some(42)) === Some(42))
        }
      }

      describe("orNull") {
        it("returns null") {
          assert(None.orNull === null)
        }
      }

      describe("seq") {
        it("returns an empty seq") {
          assert(None.seq === Seq.empty)
        }
      }

      // TODO: collect
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
        assert(Some(42).count(_ => true) === 1)
      }

      describe("contains") {
        it("tests whether the value equals the supplied argument") {
          assert(Some(42).contains(42))
          assert(!Some(42).contains(666))
        }
      }

      describe("get") {
        it("returns the value") {
          assert(Some(42).get === 42)
        }
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

      describe("orElse") {
        it("can be chained with orElse") {
          assert(None.orElse(Some(42)) === Some(42))
          assert(Some(42).orElse(Some(666)) === Some(42))
        }
      }

      // TODO: collect
    }
  }
}
