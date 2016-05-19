package com.github.nigelhp.scala

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class OptionSpec extends FunSpec {

  describe("an Optional value") {
    it("can be None") {
      None shouldBe a [Option[_]]
    }

    it("can be Some(value)") {
      Some(42) shouldBe a [Option[_]]
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
      it("can be created from a null by the apply factory method") {
        assert(Option(null) === None)
      }

      it("can be created by the empty factory method") {
        assert(Option.empty == None)
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
          assert(!None.contains(None))
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

      describe("get") {
        it("throws a NoSuchElementException") {
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

      // verbose and non-idiomatic
      it("can be pattern matched") {
        val option: Option[Int] = None
        val n: Int = option match {
          case None => 0
          case Some(x) => x
        }

        assert(n === 0)
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

      describe("map") {
        it("returns None") {
          assert(None.map(_ => 42) === None)
        }
      }

      describe("flatMap") {
        it("returns None") {
          assert(None.flatMap(_ => Some(42)) === None)
        }
      }

      describe("fold") {
        it("is equivalent to Option.map(f).getOrElse(ifEmpty) and returns the supplied default value") {
          val option: Option[Int] = None

          assert(option.fold(666) { i => i * i } === 666)
        }
      }

      describe("foldLeft") {
        it("returns the supplied start value") {
          val option: Option[Int] = None

          assert(option.foldLeft(666)((_:Int, _:Int) => 42) === 666)
        }
      }

      describe("foldRight") {
        it("returns the supplied start value") {
          val option: Option[Int] = None

          assert(option.foldRight(666)((_:Int, _:Int) => 42) === 666)
        }
      }

      describe("foreach") {
        it("will not invoke the supplied side-effecting function") {
          var count = 0

          None.foreach(_ => count += 1)

          assert(count === 0)
        }
      }

      describe("forall") {
        it("returns true regardless of the supplied predicate") {
          assert(None.forall(_ => true))
          assert(None.forall(_ => false))
        }
      }

      describe("collect") {
        it("returns None") {
          val pf: PartialFunction[Any, String] = { case s:String => "String" }

          assert(None.collect(pf) === None)
        }
      }

      // ->
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
          // TODO: eliminate compiler warning
          assert(None.orNull == null)
        }
      }

      describe("seq") {
        it("returns an empty seq") {
          assert(None.seq === Seq.empty)
        }
      }
    }

    describe("Some") {
      it("can be created from a value via the apply factory method") {
        assert(Option(42) === Some(42))
      }

      it("is defined") {
        assert(Some(42).isDefined)
      }

      it("is not empty") {
        assert(!Some(42).isEmpty)
      }

      it("is nonEmpty") {
        assert(Some(42).nonEmpty)
      }

      it("has a size of 1") {
        assert(Some(42).size === 1)
      }

      describe("count") {
        it("returns 1 if the supplied predicate returns true") {
          assert(Some(42).count(_ => true) === 1)
        }

        it("returns 0 if the supplied predicate returns false") {
          assert(Some(42).count(_ => false) === 0)
        }
      }

      describe("contains") {
        it("tests whether the value equals the supplied argument") {
          assert(Some(42).contains(42))
          assert(!Some(42).contains(666))
        }
      }

      describe("exists") {
        it("returns true when the supplied predicate is satisfied") {
          assert(Some(42).exists(_ => true))
        }

        it("returns false when the supplied predicate is not satisfied") {
          assert(!Some(42).exists(_ => false))
        }
      }

      describe("find") {
        it("returns the option when the supplied predicate is satisfied") {
          assert(Some(42).find(_ => true) === Some(42))
        }

        it("returns None when the supplied predicate is not satisfied") {
          assert(Some(42).find(_ => false) === None)
        }
      }

      describe("get") {
        it("returns the value") {
          assert(Some(42).get === 42)
        }
      }

      describe("getOrElse") {
        it("returns the value rather than the supplied default") {
          assert(Some(42).getOrElse(666) === 42)
        }
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

      describe("filter") {
        it("will return the Option if the supplied predicate is satisfied") {
          assert(Some(42).filter(_ => true) === Some(42))
        }

        it("will return None if the supplied predicate is not satisfied") {
          assert(Some(42).filter(_ => false) === None)
        }
      }

      describe("filterNot") {
        it("will return the Option if the supplied predicate is not satisfied") {
          assert(Some(42).filterNot(_ => false) === Some(42))
        }

        it("will return None if the supplied predicate is satisfied") {
          assert(Some(42).filterNot(_ => true) === None)
        }
      }

      describe("map") {
        it("returns a Some containing the result of applying the supplied function") {
          assert(Some(42).map(i => i * i) === Some(1764))
        }

        it("returns a nested Option if the supplied mapping function itself returns an Option") {
          assert(Some(42).map(Option(_)) === Some(Some(42)))
          assert(Some(42).map(_ => None) === Some(None))
        }
      }

      describe("flatMap") {
        it("returns the Option returned by the supplied mapping function") {
          assert(Some(42).flatMap(Option(_)) === Some(42))
          assert(Some(42).flatMap(_ => None) === None)
        }
      }

      describe("fold") {
        it("is equivalent to Option.map(f).getOrElse(ifEmpty) and returns the mapped value") {
          assert(Some(42).fold(666) {i => i * i} === 1764)
        }
      }

      describe("foldLeft") {
        it("returns the result of invoking the supplied function with the start value and the option value") {
          assert(Some(42).foldLeft(666)(_ + _) === 708)
        }
      }

      describe("foldRight") {
        it("returns the result of invoking the supplied function with the start value and the option value") {
          assert(Some(42).foldRight(666)(_ + _) === 708)
        }
      }

      describe("foreach") {
        it("will invoke the supplied side-effecting function once") {
          var count = 0

          Some(42).foreach(_ => count += 1)

          assert(count === 1)
        }
      }

      describe("forall") {
        it("returns the result of applying the supplied predicate to the option value") {
          assert(Some(42).forall(_ => true))
          assert(!Some(42).forall(_ => false))
        }
      }

      describe("collect") {
        val pf: PartialFunction[Any, String] = { case s:String => "String" }

        it("returns a Some containing the result of the partial function when the function is defined for the option value") {         
          assert(Some("Hello World!").collect(pf) === Some("String"))
        }

        it("returns None when the partial function is not defined for the option value") {
          assert(Some(42).collect(pf) === None)
        }
      }

      // ->
      it("can also return its value via orNull") {
        val value = "Hello World!"
        val option: Option[String] = Some(value)

        assert(option.orNull === value)
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
    }
  }
}
