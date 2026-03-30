package types

import org.scalatest.freespec.AnyFreeSpec

class AlternativeSpec extends AnyFreeSpec {
  "Alternative" - {
    val listAlternative: Alternative[List] = Alternative.from(
      Applicative.fromMap2(
        new Functor[List] {
          override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
        },
        new Point[List] {
          override def point[A](a: => A): List[A] = List(a)
        },
        new Map2[List] {
          override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] =
            fa.flatMap(a => fb.map(b => f(a, b)))
        }
      ),
      PlusEmpty.from(
        new Plus[List] {
          override def plus[A](fa1: List[A], fa2: => List[A]): List[A] = fa1 ++ fa2
        },
        new Empty[List] {
          override def empty[A]: List[A] = List.empty
        }
      )
    )

    "combine" - {
      "returns constructed value" in {
        val some = Option(1, List(2, 3))
        assert(listAlternative.combine(some) === List(1, 2, 3))
      }

      "returns empty if input is None" in {
        val none: Option[(Int, List[Int])] = None
        assert(listAlternative.combine(none) === listAlternative.empty)
      }
    }
  }
}
