package types

import org.scalatest.freespec.AnyFreeSpec

class MonadSpec extends AnyFreeSpec {
  "Monad" - {
    val optionMonad: Monad[Option] = new Monad[Option] {
      override def point[A]: (=> A) => Option[A] = Some(_)

      override def bind[A, B]: Option[A] => (A => Option[B]) => Option[B] = _.flatMap
    }

    "ap" - {
      "returns ap result" in {
        val optionA: Option[Int] = Option(21)
        val optionF: Option[Int => Int] = Option((_: Int) * 2)
        assert(optionMonad.ap(optionA)(optionF) === Option(42))
      }
    }

    "map2" - {
      "returns map2 result" in {
        val oa = Option("4")
        val ob = Option(2)
        val actual = optionMonad.map2(oa)(ob)((a, b) => (a + b.toString).toDouble)
        assert(actual === Option(42d))
      }
    }
  }
}
