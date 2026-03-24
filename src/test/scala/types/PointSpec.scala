package types

import org.scalatest.freespec.AnyFreeSpec

class PointSpec extends AnyFreeSpec {
  "Point" - {
    val optionPoint: Point[Option] = new Point[Option] {
      override def point[A](a: => A): Option[A] = Some(a)
    }

    val listPoint: Point[List] = new Point[List] {
      override def point[A](a: => A): List[A] = List(a)
    }

    "compose" - {
      "returns composed Point" in {
        val composed = Point.compose(listPoint, optionPoint)
        assert(composed.point(42) === List(Option(42)))
      }
    }
  }
}
