package types

import org.scalatest.freespec.AnyFreeSpec

class SemigroupSpec extends AnyFreeSpec {
  "Semigroup" - {
    val intSemigroup: Semigroup[Int] = new Semigroup[Int] {
      override def append(a1: Int, a2: => Int): Int = a1 + a2
    }

    "object" - {
      "fromPlus" - {
        given listPlus: Plus[List] = new Plus[List] {
          override def plus[A](fa1: List[A], fa2: => List[A]): List[A] = fa1 ++ fa2
        }

        "append" - {
          "returns concatenated list" in {
            val semigroup: Semigroup[List[Int]] = Semigroup.fromPlus
            val expected = List(1, 2, 3, 4, 5, 6)
            val actual = semigroup.append(List(1, 2, 3), List(4, 5, 6))

            assert(actual === expected)
          }
        }
      }
    }
  }
}
