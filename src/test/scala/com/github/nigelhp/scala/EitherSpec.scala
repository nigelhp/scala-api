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

    it("can be folded to a common type by applying one function to a Left, and another to a Right") {
      val left: Either[Int, String] = Left(42)
      val right: Either[Int, String] = Right("Hello World!")
      val leftFn = (i: Int) => (i * i).toString
      val rightFn = (s: String) => s.reverse

      assert(left.fold(leftFn, rightFn) === "1764")
      assert(right.fold(leftFn, rightFn) === "!dlroW olleH")
    }

    it("can expose its value as a common parent type via merge") {
      val left: Either[Int, String] = Left(42)
      val right: Either[Int, String] = Right("Hello World!")
      val leftValue: Any = left.merge
      val rightValue: Any = right.merge

      assert(leftValue === 42)
      assert(rightValue === "Hello World!")
    }

    describe("Left") {
      it("is by convention the error value, when the Either is used in an error-handling context") {
        val either: Either[Throwable, Int] = Left(new IllegalStateException())

        assert(either.isLeft)
      }

      it("will be created via cond if the supplied test is false") {
        assert(Either.cond[Int, String](test = false, left = 42, right = "Hello World!") === Left(42))
      }

      it("can be pattern matched") {
        val either: Either[Int, String] = Left(42)
        val x = either match {
          case Left(_) => "Left"
          case Right(_) => "Right"
        }

        assert(x === "Left")
      }

      it("can be swapped to a Right") {
        assert(Left(42).swap === Right(42))
      }

      it("will return its nested Either when joinLeft is applied") {
        assert(Left(Left(42)).joinLeft === Left(42))
        assert(Left(Right("Hello World!")).joinLeft === Right("Hello World!"))
      }

      it("will return itself when joinRight is applied") {
        assert(Left(Left(42)).joinRight === Left(Left(42)))
        assert(Left(Right("Hello World!")).joinRight === Left(Right("Hello World!")))
      }
    }

    describe("a left projection") {
      // can be used to selectively operate on a Left

      describe("get") {
        it("will return the value when invoked on a Left") {
          assert(Left(42).left.get === 42)
        }

        it("will throw an exception when invoked on a Right") {
          intercept[NoSuchElementException] {
            Right("Hello World!").left.get
          }
        }
      }

      describe("getOrElse") {
        it("will return the value when invoked on a Left") {
          assert(Left(42).left.getOrElse(666) === 42)
        }

        it("will return the supplied default value when invoked on a Right") {
          assert(Right("Hello World!").left.getOrElse(666) === 666)
        }
      }

      describe("toOption") {
        it("will return the value as a Some when invoked on a Left") {
          assert(Left(42).left.toOption === Some(42))
        }

        it("will return None when invoked on a Right") {
          assert(Right("Hello World!").left.toOption === None)
        }
      }

      describe("toSeq") {
        it("will return a singleton Seq containing the value when invoked on a Left") {
          assert(Left(42).left.toSeq === Seq(42))
        }

        it("will return an empty Seq when invoked on a Right") {
          assert(Right("Hello World!").left.toSeq === Seq.empty)
        }
      }

      describe("map") {
        it("will return a Left containing the result of applying the supplied function to the value, " +
          "when invoked on a Left") {
          assert(Left(42).left.map(i => i * i) === Left(1764))
        }

        it("will return the original value when invoked on a Right") {
          val either: Either[Int, String] = Right("Hello World!")

          assert(either.left.map(i => i * i) === Right("Hello World!"))
        }
      }

      describe("flatMap") {
        it("will return the Either resulting from applying the supplied function to the value, when invoked on a Left") {
          assert(Left(42).left.flatMap(i => Left(i * i)) === Left(1764))
          assert(Left(42).left.flatMap(i => Right(BigDecimal(i * i))) === Right(BigDecimal(1764)))
        }

        it("will return the original value when invoked on a Right") {
          val either: Either[Int, String] = Right("Hello World!")

          assert(either.left.flatMap(i => Left(i * i)) == Right("Hello World!"))
          assert(either.left.flatMap(i => Right(BigDecimal(i * i))) === Right("Hello World!"))
        }
      }

      describe("foreach") {
        it("will apply the supplied function when invoked on a Left") {
          var count = 0

          assert(Left(42).left.foreach(i => {count += 1; BigDecimal(i * i)}) === BigDecimal(1764))
          assert(count === 1)
        }

        it("will not apply the supplied side-effecting function when invoked on a Right") {
          var count = 0
          val either: Either[Int, String] = Right("Hello World!")

          either.left.foreach(_ => count += 1)

          assert(count === 0)
        }
      }

      it("can be used in a for comprehension") {
        val result = for {
          x <- Left("Hello").left
          y <- Left("World!").left
        } yield x.length + y.length

        assert(result === Left(11))
      }

      describe("exists") {
        it("returns the result of applying the supplied predicate to the value when invoked on a Left") {
          // note that this is the same behaviour as forall
          assert(Left(42).left.exists(_ < 43))
          assert(!Left(42).left.exists(_ > 42))
        }

        it("returns false when invoked on a Right") {
          // note that this behaviour is in contrast to forall
          val either: Either[Int, String] = Right("Hello World!")

          assert(!either.left.exists(_ => true))
        }
      }

      describe("forall") {
        it("returns the result of applying the supplied predicate to the value when invoked on a Left") {
          // note that this is the same behaviour as exists
          assert(Left(42).left.forall(_ < 43))
          assert(!Left(42).left.forall(_ > 42))
        }

        it("returns true when invoked on a Right") {
          // note that this behaviour is in contrast to exists
          val either: Either[Int, String] = Right("Hello World!")

          assert(either.left.forall(_ => false))
        }
      }

      describe("filter") {
        it("returns a Some if the supplied predicate is satisfied when invoked on a Left") {
          assert(Left(42).left.filter(_ < 43) === Some(Left(42)))
        }
        
        it("returns a None if the supplied predicate is not satisfied when invoked on a Left") {
          assert(Left(42).left.filter(_ > 42) === None)
        }

        it("returns None when invoked on a Right") {
          assert(Right("Hello World!").left.filter(_ => true) === None)
        }
      }
    }

    describe("Right") {
      it("is by convention the success value, when Either is used in an error-handling context") {
        val either: Either[Throwable, Int] = Right(42)

        assert(either.isRight)
      }

      it("will be created via cond if the supplied test is true") {
        assert(Either.cond[Int, String](test = true, left = 42, right = "Hello World!") === Right("Hello World!"))
      }

      it("can be pattern matched") {
        val either: Either[Int, String] = Right("Hello World!")
        val x = either match {
          case Left(_) => "Left"
          case Right(_) => "Right"
        }

        assert(x === "Right")
      }

      it("can be swapped to a Left") {
        Right("Hello World!").swap === Left("Hello World!")
      }

      it("will return its nested Either when joinRight is applied") {
        assert(Right(Left(42)).joinRight === Left(42))
        assert(Right(Right("Hello World!")).joinRight === Right("Hello World!"))
      }

      it("will return itself when joinLeft is applied") {
        assert(Right(Left(42)).joinLeft === Right(Left(42)))
        assert(Right(Right("Hello World!")).joinLeft === Right(Right("Hello World!")))
      }
    }

    describe("a right projection") {
      // can be used to selectively operate on a Right

      describe("get") {
        it("will return the value when invoked on a Right") {
          assert(Right("Hello World!").right.get === "Hello World!")
        }

        it("will throw an exception when invoked on a Left") {
          intercept[NoSuchElementException] {
            Left(42).right.get
          }
        }
      }

      describe("getOrElse") {
        it("will return the value when invoked on a Right") {
          assert(Right("Hello World!").right.getOrElse("Goodbye!") === "Hello World!")
        }

        it("will return the supplied default value when invoked on a Left") {
          assert(Left(42).right.getOrElse("Hello World!") === "Hello World!")
        }
      }

      describe("toOption") {
        it("will return the value as a Some when invoked on a Right") {
          assert(Right("Hello World!").right.toOption === Some("Hello World!"))
        }

        it("will return None when invoked on a Left") {
          assert(Left(42).right.toOption === None)
        }
      }

      describe("toSeq") {
        it("will return a singleton Seq containing the value when invoked on a Right") {
          assert(Right("Hello World!").right.toSeq === Seq("Hello World!"))
        }

        it("will return an empty Seq when invoked on a Left") {
          assert(Left(42).right.toSeq === Seq.empty)
        }
      }

      describe("map") {
        it("will return a Right containing the result of applying the supplied function to the value, " +
          "when invoked on a Right") {
          assert(Right("Hello World!").right.map(_.reverse) === Right("!dlroW olleH"))
        }

        it("will return the original value when invoked on a Left") {
          val either: Either[Int, String] = Left(42)

          assert(either.right.map(_.reverse) === Left(42))
        }
      }

      describe("flatMap") {
        it("will return the Either resulting from applying the supplied function to the value, when invoked on a Right") {
          assert(Right("Hello World!").right.flatMap(s => Left(s.reverse)) === Left("!dlroW olleH"))
          assert(Right("Hello World!").right.flatMap(s => Right(s.length)) === Right(12))
        }

        it("will return the original value when invoked on a Left") {
          val either: Either[Int, String] = Left(42)

          assert(either.right.flatMap(s => Left(s.reverse)) === Left(42))
        }
      }

      describe("foreach") {
        it("will apply the supplied function when invoked on a Right") {
          var count = 0

          assert(Right("Hello World!").right.foreach(s => {count += 1; s.reverse}) === "!dlroW olleH")
          assert(count === 1)
        }

        it("will not apply the supplied side-effecting function when invoked on a Left") {
          var count = 0
          val either: Either[Int, String] = Left(42)

          either.right.foreach(_ => count += 1)

          assert(count === 0)
        }
      }

      it("can be used in a for comprehension") {
        val result = for {
          x <- Right("Hello").right
          y <- Right("World!").right
        } yield x.length + y.length

        assert(result === Right(11))
      }

      describe("exists") {
        it("returns the result of applying the supplied predicate to the value when invoked on a Right") {
          // note that this is the same behaviour as forall
          assert(Right("Hello World!").right.exists(_.startsWith("Hello")))
          assert(!Right("Hello World!").right.exists(_.startsWith("Goodbye")))
        }

        it("returns false when invoked on a Left") {
          // note that this behaviour is in contrast to forall
          assert(!Left(42).right.exists(_ => true))
        }
      }

      describe("forall") {
        it("returns the result of applying the supplied predicate to the value when invoked on a Right") {
          // note that this is the same behaviour as exists
          assert(Right("Hello World!").right.forall(_.startsWith("Hello")))
          assert(!Right("Hello World!").right.forall(_.startsWith("Goodbye")))
        }

        it("returns true when invoked on a Left") {
          // note that this behaviour is in contrast to exists
          val either: Either[Int, String] = Left(42)

          assert(either.right.forall(_ => false))
        }
      }

      describe("filter") {
        it("returns a Some if the supplied predicate is satisfied when invoked on a Right") {
          assert(Right("Hello World!").right.filter(_.startsWith("Hello")) === Some(Right("Hello World!")))
        }

        it("returns a None if the supplied predicate is not satisfied when invoked on a Right") {
          assert(Right("Hello World!").right.filter(_.startsWith("Goodbye")) === None)
        }

        it("returns a None when invoked on a Left") {
          assert(Left(42).right.filter(_ => true) === None)
        }
      }
    }
  }
}
