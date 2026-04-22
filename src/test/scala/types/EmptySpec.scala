package types

import org.scalatest.freespec.AnyFreeSpec

class EmptySpec extends AnyFreeSpec {
  "Empty" - {
    val optionEmpty: Empty[Option] = new Empty[Option] {
      override def empty[A]: Option[A] = None
    }

    val listEmpty: Empty[List] = new Empty[List] {
      override def empty[A]: List[A] = List()
    }
  }
}
