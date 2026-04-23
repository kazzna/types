package types

import org.scalatest.freespec.AnyFreeSpec

class NaturalTransformationSpec extends AnyFreeSpec {
  "NaturalTransformation" - {
    "type alias ~> can be used" in {
      val optionToList: Option ~> List = new NaturalTransformation[Option, List] {
        override def apply[A](fa: Option[A]): List[A] = fa.toList
      }

      assert(optionToList(Some(1)) === List(1))
      assert(optionToList(Option.empty[Int]) === List.empty)
    }

    "can be implemented and invoked via apply" in {
      val optionToList: NaturalTransformation[Option, List] = new NaturalTransformation[Option, List] {
        override def apply[A](fa: Option[A]): List[A] = fa.toList
      }

      assert(optionToList(Some(1)) === List(1))
      assert(optionToList(Option.empty[Int]) === List.empty)
    }

    "andThen composes F ~> G with G ~> H to produce F ~> H" in {
      val optionToList: Option ~> List = new NaturalTransformation[Option, List] {
        override def apply[A](fa: Option[A]): List[A] = fa.toList
      }

      val listToVector: List ~> Vector = new NaturalTransformation[List, Vector] {
        override def apply[A](fa: List[A]): Vector[A] = fa.toVector
      }

      val vectorToSeq: Vector ~> Seq = new NaturalTransformation[Vector, Seq] {
        override def apply[A](fa: Vector[A]): Seq[A] = fa.toSeq
      }

      val composed = optionToList.andThen(listToVector).andThen(vectorToSeq)
      assert(composed(Some(1)) === Seq(1))
      assert(composed(Option.empty[Int]) === Seq.empty)
    }

    ">>> is an alias for andThen" in {
      val optionToList: Option ~> List = new NaturalTransformation[Option, List] {
        override def apply[A](fa: Option[A]): List[A] = fa.toList
      }

      val listToVector: List ~> Vector = new NaturalTransformation[List, Vector] {
        override def apply[A](fa: List[A]): Vector[A] = fa.toVector
      }

      val vectorToSeq: Vector ~> Seq = new NaturalTransformation[Vector, Seq] {
        override def apply[A](fa: Vector[A]): Seq[A] = fa.toSeq
      }

      val composed = optionToList >>> listToVector >>> vectorToSeq
      assert(composed(Some(1)) === Seq(1))
      assert(composed(Option.empty[Int]) === Seq.empty)
    }

    "compose composes G ~> H with F ~> G to produce F ~> H" in {
      val optionToList: Option ~> List = new NaturalTransformation[Option, List] {
        override def apply[A](fa: Option[A]): List[A] = fa.toList
      }

      val listToVector: List ~> Vector = new NaturalTransformation[List, Vector] {
        override def apply[A](fa: List[A]): Vector[A] = fa.toVector
      }

      val vectorToSeq: Vector ~> Seq = new NaturalTransformation[Vector, Seq] {
        override def apply[A](fa: Vector[A]): Seq[A] = fa.toSeq
      }

      val composed = vectorToSeq.compose(listToVector).compose(optionToList)
      assert(composed(Some(1)) === Seq(1))
      assert(composed(Option.empty[Int]) === Seq.empty)
    }

    "<<< is an alias for compose" in {
      val optionToList: Option ~> List = new NaturalTransformation[Option, List] {
        override def apply[A](fa: Option[A]): List[A] = fa.toList
      }

      val listToVector: List ~> Vector = new NaturalTransformation[List, Vector] {
        override def apply[A](fa: List[A]): Vector[A] = fa.toVector
      }

      val vectorToSeq: Vector ~> Seq = new NaturalTransformation[Vector, Seq] {
        override def apply[A](fa: Vector[A]): Seq[A] = fa.toSeq
      }

      val composed = vectorToSeq <<< listToVector <<< optionToList
      assert(composed(Some(1)) === Seq(1))
      assert(composed(Option.empty[Int]) === Seq.empty)
    }

    "reflect returns a NaturalTransformation that returns the same reference as the argument" in {
      val reflect = NaturalTransformation.reflect[Option]

      val a = Some(1)
      assert(reflect(a) eq a)

      val b = Option.empty[String]
      assert(reflect(b) eq b)

      val reflectList = NaturalTransformation.reflect[List]
      val c = List("a", "b", "c")
      assert(reflectList(c) eq c)
    }

    "reflect composed with a NaturalTransformation yields the same instance" - {
      val n: Option ~> List = new NaturalTransformation[Option, List] {
        override def apply[A](fa: Option[A]): List[A] = fa.toList
      }

      "reflect[F].andThen(n) eq n" in {
        assert((NaturalTransformation.reflect[Option].andThen(n)) eq n)
      }

      "n.andThen(reflect[G]) eq n" in {
        assert((n.andThen(NaturalTransformation.reflect[List])) eq n)
      }

      "n.compose(reflect[F]) eq n" in {
        assert((n.compose(NaturalTransformation.reflect[Option])) eq n)
      }

      "reflect[G].compose(n) eq n" in {
        assert((NaturalTransformation.reflect[List].compose(n)) eq n)
      }
    }
  }
}
