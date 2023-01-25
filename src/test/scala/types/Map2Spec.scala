package types

import org.scalatest.freespec.AnyFreeSpec

class Map2Spec extends AnyFreeSpec {
  "Map2" - {
    implicit def monoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
      override def zero: List[A] = List.empty

      override def append: List[A] => (=> List[A]) => List[A] = a => a ++ _
    }

    implicit def withMonoid[M](implicit M: Monoid[M]): Map2[Either[M, *]] = new Map2[Either[M, *]] {
      override def map2[A, B, C]: Either[M, A] => Either[M, B] => ((A, B) => C) => Either[M, C] =
        fa => fb => f => fa.left.map(M.append(_)(fb.map(_ => M.zero).merge)).flatMap(a => fb.map(b => f(a, b)))

      override def ap[A, B]: Either[M, A] => Either[M, A => B] => Either[M, B] = Map2.apFromMap2(map2)
    }

    "ap" - {
      "returns mapped value" in {
        val fa: Either[List[String], Int] = Right(21)
        val ff: Either[List[String], Int => String] = Right(i => (i * 2).toString)
        val map2 = Map2[Either[List[String], *]]

        assert(map2.ap(fa)(ff) === Right("42"))
      }

      "merges given errors" in {
        val fa: Either[List[String], Int] = Left(List("1st error"))
        val ff: Either[List[String], Int => String] = Left(List("2nd error"))
        val map2 = Map2[Either[List[String], *]]

        assert(map2.ap(fa)(ff) === Left(List("1st error", "2nd error")))
      }
    }
  }
}
