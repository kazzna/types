package types

import org.scalatest.freespec.AnyFreeSpec

class FlattenSpec extends AnyFreeSpec {
  "Flatten" - {
    given flatten: Flatten[List] = new Flatten[List] {
      override def flatten[A](ffa: List[List[A]]): List[A] = ffa.flatten
    }

    "flatten" - {
      "returns flatten object" in {
        val list: List[List[Int]] = List(List(1, 2, 3), List(4, 5), List(6, 7, 8))
        val expected = List(1, 2, 3, 4, 5, 6, 7, 8)
        assert(flatten.flatten(list) === expected)
      }
    }
  }
}
