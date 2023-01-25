package types

import org.scalatest.freespec.AnyFreeSpec

class AlternativeSpec extends AnyFreeSpec {
  "Alternative" - {
    val listAlternative: Alternative[List] = new Alternative[List] {
      override def point[A]: (=> A) => List[A] = List(_)

      override def ap[A, B]: List[A] => List[A => B] => List[B] = fa =>
        ff =>
          for {
            a <- fa
            f <- ff
          } yield f(a)

      override def map2[A, B, C]: List[A] => List[B] => ((A, B) => C) => List[C] =
        Alternative.map2FromPointAndAp(point, ap, ap)

      override def empty[A]: List[A] = List.empty

      override def plus[A]: List[A] => (=> List[A]) => List[A] = fa => fa ++ _
    }

    "combine" - {
      "returns constructed value" in {
        val some = Option(1, List(2, 3))
        assert(listAlternative.combine(some) === List(1, 2, 3))
      }

      "returns empty if input is None" in {
        val none: Option[(Int, List[Int])] = None
        assert(listAlternative.combine(none) === listAlternative.empty)
      }
    }
  }
}
