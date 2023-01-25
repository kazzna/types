package types

import org.scalatest.freespec.AnyFreeSpec

class PointSpec extends AnyFreeSpec {
  "Point" - {
    val optionPoint: Point[Option] = new Point[Option] {
      override def point[A]: (=> A) => Option[A] = Some(_)
    }

    val listPoint: Point[List] = new Point[List] {
      override def point[A]: (=> A) => List[A] = a => List(a)
    }

    "compose" - {
      "returns composed Point" in {
        val composed = listPoint.compose(optionPoint)
        assert(composed.point(42) === List(Option(42)))
      }
    }
  }
}
