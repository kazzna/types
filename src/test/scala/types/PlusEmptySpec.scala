package types

import org.scalatest.freespec.AnyFreeSpec

class PlusEmptySpec extends AnyFreeSpec {
  "PlusEmpty" - {
    val listPlusEmpty: PlusEmpty[List] = new PlusEmpty[List] {
      override def empty[A]: List[A] = List.empty

      override def plus[A]: List[A] => (=> List[A]) => List[A] = fa => fa ++ _
    }

    "unfoldLeft" - {
      "returns unfolded value" in {
        val seed = 3
        val f = (i: Int) =>
          if (i <= 0) None
          else Option((i - 1, LazyList.fill(i)(i.toDouble).toList))
        assert(listPlusEmpty.unfoldLeft(seed)(f) === List(1d, 2d, 2d, 3d, 3d, 3d))
      }

      "returns empty value if empty" in {
        val seed = 0
        val f = (i: Int) =>
          if (i <= 0) None
          else Option((i - 1, LazyList.fill(i)(i.toDouble).toList))
        assert(listPlusEmpty.unfoldLeft(seed)(f) === List.empty)
      }
    }

    "unfoldRight" - {
      "returns unfolded value" in {
        val seed = 3
        val f = (i: Int) =>
          if (i <= 0) None
          else Option((LazyList.fill(i)(i.toDouble).toList, i - 1))
        assert(listPlusEmpty.unfoldRight(seed)(f) === List(3d, 3d, 3d, 2d, 2d, 1d))
      }

      "returns empty value if empty" in {
        val seed = 0
        val f = (i: Int) =>
          if (i <= 0) None
          else Option((LazyList.fill(i)(i.toDouble).toList, i - 1))
        assert(listPlusEmpty.unfoldRight(seed)(f) === List.empty)
      }
    }

    "asMonoid" - {
      val monoid = listPlusEmpty.asMonoid[Int]
      "zero" - {
        "returns `empty` value" in {
          assert(monoid.zero === List.empty)
        }
      }

      "append" - {
        "returns `plus` result" in {
          assert(monoid.append(List(1, 2, 3))(List(4, 5, 6)) === List(1, 2, 3, 4, 5, 6))
        }
      }
    }
  }
}
