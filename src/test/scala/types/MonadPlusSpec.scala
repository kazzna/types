package types

import org.scalatest.freespec.AnyFreeSpec

class MonadPlusSpec extends AnyFreeSpec {
  "Monad" - {
    val monadPlus: MonadPlus[List] = new MonadPlus[List] {
      override def point[A]: (=> A) => List[A] = List(_)

      override def bind[A, B]: List[A] => (A => List[B]) => List[B] = fa => fa.flatMap(_)

      override def empty[A]: List[A] = List.empty

      override def plus[A]: List[A] => (=> List[A]) => List[A] = fa => fa ++ _
    }

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
