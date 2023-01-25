package types

import org.scalatest.freespec.AnyFreeSpec

class ApplicativeSpec extends AnyFreeSpec {
  "Applicative" - {
    implicit def monoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
      override def zero: List[A] = List.empty

      override def append: List[A] => (=> List[A]) => List[A] = fa => fa ++ _
    }

    implicit def withMonoid[M: Monoid]: Applicative[Either[M, *]] = new Applicative[Either[M, *]] {
      override def point[A]: (=> A) => Either[M, A] = Right(_)

      override def ap[A, B]: Either[M, A] => Either[M, A => B] => Either[M, B] =
        fa => ff => fa.left.map(Monoid[M].append(_)(ff.map(_ => Monoid[M].zero).merge)).flatMap(a => ff.map(_(a)))

      override def map2[A, B, C]: Either[M, A] => Either[M, B] => ((A, B) => C) => Either[M, C] =
        Applicative.map2FromPointAndAp(point, ap, ap)
    }

    val eitherApplicative = Applicative[Either[List[String], *]]

    val listApplicative: Applicative[List] = new Applicative[List] {
      override def point[A]: (=> A) => List[A] = List(_)

      override def ap[A, B]: List[A] => List[A => B] => List[B] = fa =>
        ff =>
          for {
            a <- fa
            f <- ff
          } yield f(a)

      override def map2[A, B, C]: List[A] => List[B] => ((A, B) => C) => List[C] =
        Applicative.map2FromPointAndAp(point, ap, ap)
    }

    "map" - {
      "returns mapped value" in {
        val fa: Either[List[String], Int] = Right(42)
        assert(eitherApplicative.map(fa)(_.toDouble) === Right(42d))
      }
    }

    "ap" - {
      "returns applied value" in {
        val fa: Either[List[String], Int] = Right(21)
        val ff: Either[List[String], Int => String] = Right(i => (i * 2).toString)
        assert(eitherApplicative.ap(fa)(ff) === Right("42"))
      }

      "merges given errors" in {
        val fa: Either[List[String], Int] = Left(List("1st error"))
        val ff: Either[List[String], Int => String] = Left(List("2nd error"))
        assert(eitherApplicative.ap(fa)(ff) === Left(List("1st error", "2nd error")))
      }
    }

    "map2" - {
      "returns mapped value" in {
        val fa: Either[List[String], Int] = Right(42)
        val fb: Either[List[String], String] = Right("***")
        val f = (i: Int, s: String) => s"$s$i$s"
        assert(eitherApplicative.map2(fa)(fb)(f) === Right("***42***"))
      }

      "merges given errors" in {
        val fa: Either[List[String], Int] = Left(List("1st error"))
        val fb: Either[List[String], String] = Left(List("2nd error"))
        val f = (i: Int, s: String) => s"$s$i$s"
        assert(eitherApplicative.map2(fa)(fb)(f) === Left(List("1st error", "2nd error")))
      }
    }

    "compose" - {
      val composed: Applicative[Lambda[A => List[Either[List[String], A]]]] =
        listApplicative.compose(eitherApplicative)

      "point" - {
        "returns composed point" in {
          assert(composed.point(42) === List(Right(42)))
        }
      }

      "ap" - {
        "returns composed ap" in {
          val error1 = List("error on 2nd in list1")
          val error2 = List("error on 1st in list2")
          val list1: List[Either[List[String], String]] = List(Right("1"), Left(error1), Right("42"))
          val list2: List[Either[List[String], String => Int]] =
            List(Left(error2), Right((_: String).toInt), Right((_: String).length))
          val expected = List(
            Left(List("error on 1st in list2")),
            Right(1),
            Right(1),
            Left(List("error on 2nd in list1", "error on 1st in list2")),
            Left(List("error on 2nd in list1")),
            Left(List("error on 2nd in list1")),
            Left(List("error on 1st in list2")),
            Right(42),
            Right(2)
          )
          assert(composed.ap(list1)(list2) === expected)
        }
      }

      "map2" - {
        "returns composed ap" in {
          val error1 = List("error on 2nd in list1")
          val error2 = List("error on 1st in list2")
          val list1 = List(Right(1), Left(error1), Right(3))
          val list2 = List(Left(error2), Right("*"), Right("-"))
          val f = (i: Int, s: String) => s"$s$i$s"
          val expected = List(
            Left(error2),
            Right("*1*"),
            Right("-1-"),
            Left(error1 ++ error2),
            Left(error1),
            Left(error1),
            Left(error2),
            Right("*3*"),
            Right("-3-")
          )
          assert(composed.map2(list1)(list2)(f) === expected)
        }
      }
    }

    "map3" - {
      "returns mapped value" in {
        val fa: Either[List[String], Int] = Right(21)
        val fb: Either[List[String], String] = Right("***")
        val fc: Either[List[String], Double] = Right(21d)
        val f = (i: Int, s: String, d: Double) => s"$s${i + d}$s"
        assert(eitherApplicative.map3(fa)(fb)(fc)(f) === Right("***42.0***"))
      }

      "merges given errors" in {
        val fa: Either[List[String], Int] = Left(List("1st error"))
        val fb: Either[List[String], String] = Left(List("2nd error"))
        val fc: Either[List[String], Double] = Left(List("3rd error"))
        val f = (i: Int, s: String, d: Double) => s"$s${i + d}$s"
        assert(eitherApplicative.map3(fa)(fb)(fc)(f) === Left(List("1st error", "2nd error", "3rd error")))
      }

      "skips value when error occurred" in {
        val fa: Either[List[String], Int] = Left(List("1st error"))
        val fb: Either[List[String], String] = Right("abc")
        val fc: Either[List[String], Double] = Left(List("3rd error"))
        val f = (i: Int, s: String, d: Double) => s"$s${i + d}$s"
        assert(eitherApplicative.map3(fa)(fb)(fc)(f) === Left(List("1st error", "3rd error")))
      }
    }

    "map4" - {
      "returns mapped value" in {
        val fa: Either[List[String], Int] = Right(21)
        val fb: Either[List[String], String] = Right("***")
        val fc: Either[List[String], Double] = Right(21d)
        val fd: Either[List[String], Int] = Right(2)
        val f = (i: Int, s: String, d: Double, j: Int) => LazyList.fill(j)(s"$s${i + d}$s").mkString("")
        assert(eitherApplicative.map4(fa)(fb)(fc)(fd)(f) === Right("***42.0******42.0***"))
      }

      "merges given errors" in {
        val fa: Either[List[String], Int] = Left(List("1st error"))
        val fb: Either[List[String], String] = Left(List("2nd error"))
        val fc: Either[List[String], Double] = Left(List("3rd error"))
        val fd: Either[List[String], Int] = Left(List("4th error"))
        val f = (i: Int, s: String, d: Double, j: Int) => LazyList.fill(j)(s"$s${i + d}$s").mkString("")
        val expected = Left(List("1st error", "2nd error", "3rd error", "4th error"))
        assert(eitherApplicative.map4(fa)(fb)(fc)(fd)(f) === expected)
      }

      "skips value when error occurred" in {
        val fa: Either[List[String], Int] = Left(List("1st error"))
        val fb: Either[List[String], String] = Right("abc")
        val fc: Either[List[String], Double] = Left(List("3rd error"))
        val fd: Either[List[String], Int] = Left(List("4th error"))
        val f = (i: Int, s: String, d: Double, j: Int) => LazyList.fill(j)(s"$s${i + d}$s").mkString("")
        val expected = Left(List("1st error", "3rd error", "4th error"))
        assert(eitherApplicative.map4(fa)(fb)(fc)(fd)(f) === expected)
      }
    }

    "map5" - {
      "returns mapped value" in {
        val fa: Either[List[String], Int] = Right(21)
        val fb: Either[List[String], String] = Right("***")
        val fc: Either[List[String], Double] = Right(21d)
        val fd: Either[List[String], Int] = Right(2)
        val fe: Either[List[String], Boolean] = Right(false)
        val f = (i: Int, s: String, d: Double, j: Int, b: Boolean) => LazyList.fill(j)(s"$s${i + d}$b").mkString("")
        val expected = Right("***42.0false***42.0false")
        val actual = eitherApplicative.map5(fa)(fb)(fc)(fd)(fe)(f)
        assert(actual === expected)
      }

      "merges given errors" in {
        val fa: Either[List[String], Int] = Left(List("1st error"))
        val fb: Either[List[String], String] = Left(List("2nd error"))
        val fc: Either[List[String], Double] = Left(List("3rd error"))
        val fd: Either[List[String], Int] = Left(List("4th error"))
        val fe: Either[List[String], Boolean] = Left(List("5th error"))
        val f = (i: Int, s: String, d: Double, j: Int, b: Boolean) => LazyList.fill(j)(s"$s${i + d}$b").mkString("")
        val expected = Left(List("1st error", "2nd error", "3rd error", "4th error", "5th error"))
        val actual = eitherApplicative.map5(fa)(fb)(fc)(fd)(fe)(f)
        assert(actual === expected)
      }

      "skips value when error occurred" in {
        val fa: Either[List[String], Int] = Left(List("1st error"))
        val fb: Either[List[String], String] = Right("abc")
        val fc: Either[List[String], Double] = Left(List("3rd error"))
        val fd: Either[List[String], Int] = Right(3)
        val fe: Either[List[String], Boolean] = Left(List("5th error"))
        val f = (i: Int, s: String, d: Double, j: Int, b: Boolean) => LazyList.fill(j)(s"$s${i + d}$b").mkString("")
        val expected = Left(List("1st error", "3rd error", "5th error"))
        val actual = eitherApplicative.map5(fa)(fb)(fc)(fd)(fe)(f)
        assert(actual === expected)
      }
    }

    "map6" - {
      "returns mapped value" in {
        val fa: Either[List[String], Int] = Right(21)
        val fb: Either[List[String], String] = Right("***")
        val fc: Either[List[String], Double] = Right(21d)
        val fd: Either[List[String], Int] = Right(2)
        val fe: Either[List[String], Boolean] = Right(false)
        val fg: Either[List[String], Char] = Right('!')
        val f = (i: Int, s: String, d: Double, j: Int, b: Boolean, c: Char) =>
          LazyList.fill(j)(s"$s${i + d}$b$c").mkString("")
        val expected = Right("***42.0false!***42.0false!")
        val actual = eitherApplicative.map6(fa)(fb)(fc)(fd)(fe)(fg)(f)
        assert(actual === expected)
      }

      "merges given errors" in {
        val fa: Either[List[String], Int] = Left(List("1st error"))
        val fb: Either[List[String], String] = Left(List("2nd error"))
        val fc: Either[List[String], Double] = Left(List("3rd error"))
        val fd: Either[List[String], Int] = Left(List("4th error"))
        val fe: Either[List[String], Boolean] = Left(List("5th error"))
        val fg: Either[List[String], Char] = Left(List("6th error"))
        val f = (i: Int, s: String, d: Double, j: Int, b: Boolean, c: Char) =>
          LazyList.fill(j)(s"$s${i + d}$b$c").mkString("")
        val expected = Left(List("1st error", "2nd error", "3rd error", "4th error", "5th error", "6th error"))
        val actual = eitherApplicative.map6(fa)(fb)(fc)(fd)(fe)(fg)(f)
        assert(actual === expected)
      }

      "skips value when error occurred" in {
        val fa: Either[List[String], Int] = Left(List("1st error"))
        val fb: Either[List[String], String] = Right("abc")
        val fc: Either[List[String], Double] = Left(List("3rd error"))
        val fd: Either[List[String], Int] = Right(3)
        val fe: Either[List[String], Boolean] = Left(List("5th error"))
        val fg: Either[List[String], Char] = Left(List("6th error"))
        val f = (i: Int, s: String, d: Double, j: Int, b: Boolean, c: Char) =>
          LazyList.fill(j)(s"$s${i + d}$b$c").mkString("")
        val expected = Left(List("1st error", "3rd error", "5th error", "6th error"))
        val actual = eitherApplicative.map6(fa)(fb)(fc)(fd)(fe)(fg)(f)
        assert(actual === expected)
      }
    }
  }
}
