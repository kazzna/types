package types

import org.scalatest.freespec.AnyFreeSpec

class Map2Spec extends AnyFreeSpec {
  "Map2" - {
    implicit def monoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
      override def zero: List[A] = List.empty

      override def append(a1: List[A], a2: => List[A]): List[A] = a1 ++ a2
    }

    given withMonoid[E](using monoid: Monoid[E]): Map2[[A] =>> Either[E, A]] = new Map2[[A] =>> Either[E, A]] {
      override def map2[A, B, C](fa: Either[E, A], fb: Either[E, B])(f: (A, B) => C): Either[E, C] =
        fa.left.map(monoid.append(_, fb.map(_ => monoid.zero).merge)).flatMap(a => fb.map(b => f(a, b)))
    }

    "ap" - {
      "returns mapped value" in {
        val fa: Either[List[String], Int] = Right(21)
        val ff: Either[List[String], Int => String] = Right(i => (i * 2).toString)
        val map2 = Map2[[A] =>> Either[List[String], A]]

        assert(Apply.fromMap2(using map2).ap(fa)(ff) === Right("42"))
      }

      "merges given errors" in {
        val fa: Either[List[String], Int] = Left(List("1st error"))
        val ff: Either[List[String], Int => String] = Left(List("2nd error"))
        val map2 = Map2[[A] =>> Either[List[String], A]]

        assert(Apply.fromMap2(using map2).ap(fa)(ff) === Left(List("1st error", "2nd error")))
      }
    }
  }
}
