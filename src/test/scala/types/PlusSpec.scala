package types

import org.scalatest.freespec.AnyFreeSpec

class PlusSpec extends AnyFreeSpec {
  "Plus" - {
    val listPlus: Plus[List] = new Plus[List] {
      override def plus[A]: List[A] => (=> List[A]) => List[A] = fa => fa ++ _
    }

    "unfoldLeftOption" - {
      "returns unfolded value" in {
        val seed = 3
        val f = (i: Int) =>
          if (i <= 0) None
          else Option((i - 1, LazyList.fill(i)(i.toDouble).toList))
        assert(listPlus.unfoldLeftOption(seed)(f) === Some(List(1d, 2d, 2d, 3d, 3d, 3d)))
      }

      "returns None if empty" in {
        val seed = 0
        val f = (i: Int) =>
          if (i <= 0) None
          else Option((i - 1, LazyList.fill(i)(i.toDouble).toList))
        assert(listPlus.unfoldLeftOption(seed)(f) === None)
      }
    }

    "unfoldRightOption" - {
      "returns unfolded value" in {
        val seed = 3
        val f = (i: Int) =>
          if (i <= 0) None
          else Option((LazyList.fill(i)(i.toDouble).toList, i - 1))
        assert(listPlus.unfoldRightOption(seed)(f) === Some(List(3d, 3d, 3d, 2d, 2d, 1d)))
      }

      "returns None if empty" in {
        val seed = 0
        val f = (i: Int) =>
          if (i <= 0) None
          else Option((LazyList.fill(i)(i.toDouble).toList, i - 1))
        assert(listPlus.unfoldRightOption(seed)(f) === None)
      }
    }

    "asSemigroup" - {
      "append" - {
        "returns `plus` result" in {
          val semigroup = listPlus.asSemigroup[Int]
          val fa = List(1, 2, 3)
          val fb = List(4, 5, 6)
          assert(semigroup.append(fa)(fb) === List(1, 2, 3, 4, 5, 6))
        }
      }
    }
  }
}
