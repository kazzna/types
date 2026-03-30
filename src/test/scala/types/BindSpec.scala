package types

import org.scalatest.freespec.AnyFreeSpec

class BindSpec extends AnyFreeSpec {
  "Bind" - {
    given bind: Bind[List] = new Bind[List] {
      override def bind[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    }

    "bind" - {
      "returns bound object" in {
        val fa: List[Int] = List(1, 2, 3)
        val f: Int => List[Long] = i => List(i * 1, i * 2, i * 3)
        val expected: List[Double] = List(1, 2, 3, 2, 4, 6, 3, 6, 9)
        assert(bind.bind(fa)(f) === expected)
      }
    }
  }
}
