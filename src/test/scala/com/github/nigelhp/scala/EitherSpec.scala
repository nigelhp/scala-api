package com.github.nigelhp.scala

import org.scalatest.FunSpec

class EitherSpec extends FunSpec {

  describe("an Either value") {
    it("can be Left") {
      val either: Either[Int, String] = Left(42)

      assert(either === Left(42))
      assert(either.isLeft)
      assert(!either.isRight)
    }

    it("can be Right") {
      val either: Either[Int, String] = Right("Hello World!")

      assert(either == Right("Hello World!"))
      assert(either.isRight)
      assert(!either.isLeft)
    }

    it("can flatten to a nested Left with joinLeft") {
      Left(Left(42)).joinLeft === Left(42)
      Right(Left(42)).joinLeft === Left(42)
    }

    it("can flatten to a nested Right with joinRight") {
      Left(Right("Hello World!")).joinRight === Right("Hello World!")
      Right(Right("Hello World!")).joinRight === Right("Hello World!")
    }

    it("will not flatten the option via join if the nested option has the wrong side") {
      Left(Right("Hello World!")).joinLeft === Left(Right("Hello World!"))
      Right(Right("Hello World!")).joinLeft === Right(Right("Hello World!"))

      Left(Left(42)).joinRight === Left(Left(42))
      Right(Left(42)).joinRight === Right(Left(42))
    }

    it("can be used in a for comprehension") {
      // note the need to use projections in the generator expressions
      val result = for {
        x <- Left("Hello").left
        y <- Left("World!").left
      } yield x.length + y.length

      assert(result === Left(11))
    }

    describe("Left") {
      it("is by convention the error value, when the Either is used in an error-handling context") {
        val either: Either[Throwable, Int] = Left(new IllegalStateException())

        assert(either.isLeft)
      }

      it("can be pattern matched") {
        val either: Either[Int, String] = Left(42)
        val x = either match {
          case Left(_) => "Left"
          case Right(_) => "Right"
        }

        assert(x === "Left")
      }

      it("will return a new Left with the result of applying the supplied function to the original value, " +
        "when map is applied to a left projection") {
        val either: Either[Int, String] = Left(42)

        assert(either.left.map(i => i * i) === Left(1764))
      }

      it("will return the original value unchanged when map is applied to a right projection") {
        val either: Either[Int, String] = Left(42)

        assert(either.right.map(_.reverse) === Left(42))
      }

      it("will return the new Either returned by the supplied function, " +
        "when flatmap is applied to a left projection") {
        val either: Either[Int, String] = Left(42)

        assert(either.left.flatMap(i => Left(i * i)) === Left(1764))
        assert(either.left.flatMap(i => Right(BigDecimal(i * i))) === Right(BigDecimal(1764)))
      }

      it("will return the original value unchanged when flatmap is applied to a right projection") {
        val either: Either[Int, String] = Left(42)

        assert(either.right.flatMap(s => Left("Hello World!")) === Left(42))
        assert(either.right.flatMap(s => Right("Hello World!")) === Left(42))
      }

      it("can be converted to an option via a projection, " +
        "returning a Some if the projection has the same side as the either's value, else None") {
        val either: Either[Int, String] = Left(42)

        assert(either.left.toOption === Some(42))
        assert(either.right.toOption === None)
      }

      it("can be converted to a seq via a projection, " +
        "returning a singleton seq if the projection has the same side as the either's value, else empty") {
        val either: Either[Int, String] = Left(42)

        assert(either.left.toSeq === Seq(42))
        assert(either.right.toSeq === Seq.empty[String])
      }

      it("can apply one function to a left value and another to a right value via fold, " +
        "as long as they have the same return type") {
        val either: Either[Int, String] = Left(42)

        assert(either.fold[String]((i: Int) => (i * i).toString, (s: String) => s.reverse) === "1764")
      }
    }

    describe("Right") {
      it("is by convention the success value, when Either is used in an error-handling context") {
        val either: Either[Throwable, Int] = Right(42)

        assert(either.isRight)
      }

      it("can be pattern matched") {
        val either: Either[Int, String] = Right("Hello World!")
        val x = either match {
          case Left(_) => "Left"
          case Right(_) => "Right"
        }

        assert(x === "Right")
      }

      it("will return a new Right with the result of applying the supplied function to the original value, " +
        "when map is applied to a right projection") {
        val either: Either[Int, String] = Right("Hello World!")

        assert(either.right.map(_.reverse) === Right("!dlroW olleH"))
      }

      it("will return the original value unchanged when map is applied to a left projection") {
        val either: Either[Int, String] = Right("Hello World!")

        assert(either.left.map(i => i * i) === Right("Hello World!"))
      }

      it("will return the new Either returned by the supplied function, " +
        "when flatmap is applied to a right projection") {
        val either: Either[Int, String] = Right("Hello World!")

        assert(either.right.flatMap(s => Left(BigInt(s.length))) === Left(BigInt(12)))
        assert(either.right.flatMap(s => Right(s.reverse)) === Right("!dlroW olleH"))
      }

      it("will return the original value unchanged when flatmap is applied to a left projection") {
        val either: Either[Int, String] = Right("Hello World!")

        assert(either.left.flatMap(s => Left(42)) === Right("Hello World!"))
        assert(either.left.flatMap(s => Right(42)) === Right("Hello World!"))
      }

      it("can be converted to an option via a projection, " +
        "returning a Some if the projection has the same side as the either's value, else None") {
        val either: Either[Int, String] = Right("Hello World!")

        assert(either.left.toOption === None)
        assert(either.right.toOption === Some("Hello World!"))
      }

      it("can be converted to a seq via a projection, " +
        "returning a singleton seq if the projection has the same side as the either's value, else empty") {
        val either: Either[Int, String] = Right("Hello World!")

        assert(either.left.toSeq === Seq.empty[Int])
        assert(either.right.toSeq === Seq("Hello World!"))
      }

      it("can apply one function to a left value and another to a right value via fold, " +
        "as long as they have the same return type") {
        val either: Either[Int, String] = Right("Hello World!")

        assert(either.fold[String]((i: Int) => (i * i).toString, (s: String) => s.reverse) === "!dlroW olleH")
      }
    }
  }
}
