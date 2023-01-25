package types

import org.scalatest.freespec.AnyFreeSpec

class BindSpec extends AnyFreeSpec {
  "Bind" - {
    val bind: Bind[List] = new Bind[List] {
      override def bind[A, B]: List[A] => (A => List[B]) => List[B] = fa => fa.flatMap(_)
    }

    "flatten" - {
      "returns flatten object" in {
        val list: List[List[Int]] = List(List(1, 2, 3), List(4, 5), List(6, 7, 8))
        val expected = List(1, 2, 3, 4, 5, 6, 7, 8)
        assert(bind.flatten(list) === expected)
      }
    }
  }
}
