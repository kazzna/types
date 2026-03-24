package types

import org.scalatest.freespec.AnyFreeSpec

class GroupSpec extends AnyFreeSpec {
  "Group" - {
    val intGroup: Group[Int] = new Group[Int] {
      override def append(a1: Int, a2: => Int): Int = a1 + a2

      override def inverse(a: Int): Int = -a

      override def zero: Int = 0
    }
  }
}
