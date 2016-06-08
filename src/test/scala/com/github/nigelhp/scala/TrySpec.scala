package com.github.nigelhp.scala

import scala.util.{Try, Success, Failure}

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class TrySpec extends FunSpec {

  describe("a Try value") {
    it("can be a Success") {
      Try("Hello World!") shouldBe a [Success[_]]
    }

    it("can be a Failure") {
      Try(throw new IllegalStateException("error message")) shouldBe a [Failure[_]]
    }

    it("will catch only non-fatal exceptions") { 
      intercept [OutOfMemoryError] {
        Try(throw new OutOfMemoryError("fatal exception"))
      }
    }

    describe("flatten") {
      it("collapses a nested Success into a Success") {
        val nested: Try[Try[String]] = Try(Try("Hello World!"))

        val flat = nested.flatten

        flat shouldBe a [Success[_]]
        assert(flat.get === "Hello World!")
      }

      it("collapses a nested Failure into a Failure") {
        val nested: Try[Try[String]] = Try(Try(throw new IllegalStateException("error message")))

        val flat = nested.flatten

        flat shouldBe a [Failure[_]]
        the [IllegalStateException] thrownBy {
          flat.get
        } should have message ("error message")
      }
    }
  }

  describe("Success") {
    val success = Try("Hello World!")

    it("is success") {
      assert(success.isSuccess)
    }

    it("is not a failure") {
      assert(!success.isFailure)
    }

    it("can be pattern matched") {
      val value = success match {
        case Success(s) => s
        case _ => ""
      }

      assert(value === "Hello World!")
    }

    describe("get") {
      it("returns the value") {
        assert(success.get === "Hello World!")
      }
    }

    describe("getOrElse") {
      it("returns the value rather than the supplied default") {
        assert(success.getOrElse("Goodbye cruel World!") === "Hello World!")
      }
    }

    describe("filter") {
      it("returns this Success when the supplied predicate is satisfied") {
        assert(success.filter(_ => true) === success)
      }

      it("returns Failure when the supplied predicate is not satisfied") {
        val failure = success.filter(_ => false)

        failure shouldBe a [Failure[_]]
        the [NoSuchElementException] thrownBy {
          failure.get
        } should have message ("Predicate does not hold for Hello World!")
      }
    }

    describe("map") {
      it("returns a value returned by the supplied mapping function as a Success") {
        assert(success.map(_.reverse) === Success("!dlroW olleH"))
      }

      it("returns an exception thrown by the supplied mapping function as a Failure") {
        val failure = success.map(_ => throw new RuntimeException("error message"))

        failure shouldBe a [Failure[_]]
        the [RuntimeException] thrownBy {
          failure.get
        } should have message ("error message")
      }
    }

    describe("flatMap") {
      it("returns a Success when the supplied mapping function returns a Success") {
        assert(success.flatMap(s => Try(s.reverse)) === Success("!dlroW olleH"))
      }

      it("returns a Failure when the supplied mapping function returns a Failure") {
        val failure = success.flatMap(s => Try(throw new RuntimeException("flatMap failure")))

        failure shouldBe a [Failure[_]]
        the [RuntimeException] thrownBy {
          failure.get
        } should have message ("flatMap failure")
      }
    }

    describe("foreach") {
      it("applies the supplied side-effecting function to this Success") {
        var x = ""

        success.foreach(s => x = s.reverse)

        assert(x === "!dlroW olleH")
      }
    }

    describe("failed") {
      it("inverts this Success into a Failure") {
        val failure = success.failed

        failure shouldBe a [Failure[_]]
        the [UnsupportedOperationException] thrownBy {
          failure.get
        } should have message ("Success.failed")
      }
    }

    describe("recover") {
      it("returns this Success") {
        val pf: PartialFunction[Throwable, String] = { case _ => "Error!" }

        assert(success.recover(pf) === success)
      }
    }

    describe("recoverWith") {
      it("returns this Success") {
        val pf: PartialFunction[Throwable, Try[String]] = { case _ => Try("Error!") }

        assert(success.recoverWith(pf) === success)
      }
    }

    describe("orElse") {
      it("returns this Success") {
        assert(success.orElse(Try("Goodbye cruel World!")) === success)
      }
    }

    describe("toOption") {
      it("returns a Some containing the value of this Success") {
        assert(success.toOption === Some("Hello World!"))
      }
    }

    describe("transform") {
      val ff = ((f: Throwable) => Try(throw new RuntimeException("transformed failure")))

      it("returns Success when the supplied success transformation function returns Success") {
        val sf = ((s: String) => Try("success"))

        val transformed = success.transform(sf, ff)

        assert(transformed === Success("success"))
      }

      it("returns Failure when the supplied success transformation function returns Failure") {
        val sf = ((s: String) => Try(throw new RuntimeException("transformed success")))

        val transformed = success.transform(sf, ff)

        transformed shouldBe a [Failure[_]]
        the [RuntimeException] thrownBy {
          transformed.get
        } should have message ("transformed success")
      }
    }

    it("supports for comprehension") {
      val result = (for {
        s <- Try("Hello World!")
        r = s.reverse
      } yield r)

      assert(result === Success("!dlroW olleH"))
    }
  }

  describe("Failure") {
    val failure = Try(throw new IllegalStateException("error message"))

    it("is not success") {
      assert(!failure.isSuccess)
    }

    it("is a failure") {
      assert(failure.isFailure)
    }

    it("can be pattern matched") {
      val msg = failure match {
        case Failure(t) => t.getMessage
        case _ => ""
      }

      assert(msg === "error message")
    }

    describe("get") {
      it("throws the exception") {
        intercept [IllegalStateException] {
          failure.get
        }
      }
    }

    describe("getOrElse") {
      it("returns the supplied default value") {
        assert(failure.getOrElse("Goodbye cruel World!") === "Goodbye cruel World!")
      }
    }

    describe("filter") {
      it("returns this Failure when the supplied predicate is satisfied") {
        assert(failure.filter(_ => true) === failure)
      }

      it("returns this Failure when the supplied predicate is not satisfied") {
        assert(failure.filter(_ => false) === failure)
      }
    }

    describe("map") {
      it("returns this Failure") {
        assert(failure.map(_ => "Hello World!") === failure)
      }      
    }

    describe("flatMap") {
      it("returns this Failure") {
        assert(failure.flatMap(_ => Try("Hello World!")) === failure)
      }
    }

    describe("foreach") {
      it("does not apply the supplied side-effecting function to this Failure") {
        var x = ""

        failure.foreach(_ => x = "Hello World!")

        assert(x === "")
      }
    }

    describe("failed") {
      it("inverts this Failure into a Success") {
        val success = failure.failed

        success shouldBe a [Success[_]]
        success.get shouldBe a [IllegalStateException]
        success.get should have message "error message"
      }
    }

    describe("recover") {
      it("returns a Success when the supplied partial function returns a value") {
        val pf: PartialFunction[Throwable, String] = { case _ => "Error!" }

        assert(failure.recover(pf) === Success("Error!"))
      }

      it("returns a Failure when the supplied partial function throws an Exception") {
        val pf: PartialFunction[Throwable, String] = { case _ => throw new RuntimeException("message") }

        val recovered = failure.recover(pf)

        recovered shouldBe a [Failure[_]]
        the [RuntimeException] thrownBy {
          recovered.get
        } should have message ("message")
      }

      it("returns the existing Failure when the supplied partial function has no match") {
        val pf: PartialFunction[Throwable, String] = { case e: IllegalArgumentException => "bad arg"}

        val recovered = failure.recover(pf)

        recovered shouldBe a [Failure[_]]
        the [IllegalStateException] thrownBy {
          recovered.get
        } should have message ("error message")
      }
    }

    describe("recoverWith") {
      it("returns a Success when the supplied partial function returns Success") {
        val pf: PartialFunction[Throwable, Try[String]] = { case _ => Try("recovered") }

        assert(failure.recoverWith(pf) === Success("recovered"))
      }

      it("returns a Failure when the supplied partial function returns a Failure") {
        val pf: PartialFunction[Throwable, Try[String]] = { case _ => Try(throw new RuntimeException("recovered")) }

        val recovered = failure.recoverWith(pf)

        recovered shouldBe a [Failure[_]]
        the [RuntimeException] thrownBy {
          recovered.get
        } should have message ("recovered")
      }

      it("returns the existing Failure when the supplied partial function has no match") {
        val pf: PartialFunction[Throwable, Try[String]] = { case e: IllegalArgumentException => Try("illegal") }

        val recovered = failure.recoverWith(pf)

        recovered shouldBe a [Failure[_]]
        the [IllegalStateException] thrownBy {
          recovered.get
        } should have message ("error message")
      }
    }

    describe("orElse") {
      it("returns the supplied default when that default is a Success") {
        assert(failure.orElse(Try("Hello World!")) === Success("Hello World!"))
      }

      it("returns the supplied default when that default is a Failure") {
        val defaulted = failure.orElse(Try(throw new RuntimeException("default")))

        defaulted shouldBe a [Failure[_]]
        the [RuntimeException] thrownBy {
          defaulted.get
        } should have message ("default")
      }
    }

    describe("toOption") {
      it("returns a None") {
        assert(failure.toOption === None)
      }
    }

    describe("transform") {
      val sf = ((s: String) => Try("transformed success"))

      it("returns Success when the supplied failure transformation function returns Success") {
        val ff = ((f: Throwable) => Try("transformed failure"))

        val transformed = failure.transform(sf, ff)

        assert(transformed === Success("transformed failure"))
      }

      it("returns Failure when the supplied failure transformation function returns Failure") {
        val ff = ((f: Throwable) => Try(throw new RuntimeException("transformed failure")))

        val transformed = failure.transform(sf, ff)

        transformed shouldBe a [Failure[_]]
        the [RuntimeException] thrownBy {
          transformed.get
        } should have message ("transformed failure")
      }
    }

    describe("for comprehension") {
      it("skips subsequent expressions on encountering a Failure") {
        val result = (for {
          s: String <- Try("Hello World!".substring(20))
          r = s.reverse
        } yield r)

        result shouldBe a [Failure[_]]
        a [StringIndexOutOfBoundsException] should be thrownBy {
          result.get
        }
      }
    }
  }
}
