package types

import org.scalatest.freespec.AnyFreeSpec

class ApplySpec extends AnyFreeSpec {
  "Apply" - {
    val optionApply: Apply[Option] = new Apply[Option] {
      def ap[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] = fa.flatMap(a => ff.map(_(a)))
    }

    val listApply: Apply[List] = new Apply[List] {
      def ap[A, B](fa: List[A])(ff: List[A => B]): List[B] = fa.flatMap(a => ff.map(_(a)))
    }
  }
}
