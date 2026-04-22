package types

import org.scalatest.freespec.AnyFreeSpec

class MonoidSpec extends AnyFreeSpec {
  "Monoid" - {
    val intMonoid: Monoid[Int] = new Monoid[Int] {
      override def append(a1: Int, a2: => Int): Int = a1 + a2
      override def zero: Int = 0
    }
  }
}
