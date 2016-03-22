package com.github.nigelhp.scala

import org.scalatest.FunSpec

class EitherSpec extends FunSpec {

  describe("an Either value") {
    it("can be Left") {
      val either: Either[Int, String] = Left(42)

      assert(either === Left(42))
      assert(either.isLeft)
    }

    it("can be Right") {
      val either: Either[Int, String] = Right("Hello World!")

      assert(either == Right("Hello World!"))
      assert(either.isRight)
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
    }
  }
}
