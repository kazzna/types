package types

import org.scalatest.freespec.AnyFreeSpec

class FunctorSpec extends AnyFreeSpec {
  "Functor" - {
    val optionFunctor: Functor[Option] = new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: (A => B)): Option[B] = fa.map(f)
    }

    val listFunctor: Functor[List] = new Functor[List] {
      override def map[A, B](fa: List[A])(f: (A => B)): List[B] = fa.map(f)
    }

    "pair" - {
      "returns paired value" in {
        val list = List(1, 2, 3, 4, 5)
        val expected = List((1, 1), (2, 2), (3, 3), (4, 4), (5, 5))
        assert(listFunctor.pair(list) === expected)
      }
    }

    "void" - {
      "returns void value" in {
        val list = List(1, 2, 3, 4, 5)
        assert(listFunctor.void(list) === List((), (), (), (), ()))
      }
    }

    "compose" - {
      "returns composed functor" in {
        val composed = Functor.compose(listFunctor, optionFunctor)
        val list = List(Some(1), Some(2), None, Some(4), Some(5))
        val expected = List(Some(1d), Some(2d), None, Some(4d), Some(5d))
        assert(composed.map(list)(_.toDouble) === expected)
      }
    }
  }
}
