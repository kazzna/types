package types

import org.scalatest.freespec.AnyFreeSpec

class MonadPlusSpec extends AnyFreeSpec {
  "Monad" - {
    val monadPlus: MonadPlus[List] = MonadPlus.from(
      Monad.fromPoint(
        new Point[List] {
          override def point[A](a: => A): List[A] = List(a)
        },
        new Bind[List] {
          override def bind[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
        }
      ),
      new PlusEmpty[List] {
        override def empty[A]: List[A] = List.empty

        override def plus[A](fa1: List[A], fa2: => List[A]): List[A] = fa1 ++ fa2
      }
    )

    "filter" - {
      "returns filtered value" in {
        val list = List(1, 2, 3, 4, 5, 6)
        val expected = List(1, 2, 4, 5)
        val actual = monadPlus.filter(list)(_ % 3 != 0)
        assert(actual === expected)
      }
    }
  }
}
