package types

import org.scalatest.freespec.AnyFreeSpec

class MonadLogicSpec extends AnyFreeSpec {
  "MonadLogic" - {
    val monadLogic: MonadLogic[LazyList] = new MonadLogic[LazyList] {
      override def point[A]: (=> A) => LazyList[A] = LazyList(_)
      override def bind[A, B]: LazyList[A] => (A => LazyList[B]) => LazyList[B] = fa => fa.flatMap(_)
      override def empty[A]: LazyList[A] = LazyList.empty
      override def plus[A]: LazyList[A] => (=> LazyList[A]) => LazyList[A] = fa => fa ++ _
      override def split[A]: LazyList[A] => LazyList[Option[(A, LazyList[A])]] =
        list => LazyList(list.headOption.map(head => (head, list.tail)))
    }

    "interleave" - {
      "returns interleaved value" in {
        val a = LazyList("a", "b", "c")
        val b = LazyList("x", "y", "z")
        val expected = LazyList("a", "x", "b", "y", "c", "z")
        val actual = monadLogic.interleave(a)(b)
        assert(actual === expected)
      }

      "returns remaining list when one of list exhausted" in {
        val a = LazyList(1, 2, 3, 4)
        val b = LazyList(10)
        val expected = LazyList(1, 10, 2, 3, 4)
        val actual = monadLogic.interleave(a)(b)
        assert(actual === expected)
      }
    }

    "bindLike" - {
      val fa = LazyList(3, 11)
      val f = (i: Int) => LazyList(i, i * i)
      val g = (i: Int) => LazyList(i, i - 1)

      "returns computed value" in {
        val expected = LazyList(3, 11, 9, 121, 2, 10, 8, 120)
        assert(monadLogic.bindLike(fa)(i => monadLogic.bindLike(f(i))(g)) === expected)
      }

      "returns different value when compute order is different" in {
        val expected = LazyList(3, 11, 2, 9, 10, 121, 8, 120)
        assert(monadLogic.bindLike(monadLogic.bindLike(fa)(f))(g) === expected)
      }
    }

    "once" - {
      "returns first item of value" in {
        val a = LazyList(1, 2, 3, 4)
        assert(monadLogic.once(a) === LazyList(1))
      }
    }

    "not" - {
      "returns empty value" in {
        val a = LazyList(1, 2, 3, 4)
        assert(monadLogic.not(a) === LazyList.empty)
      }
    }

    "bindOrElse" - {
      "returns bind result if not empty" in {
        val a = monadLogic.point(42)
        val f = (i: Int) => LazyList.fill(i)(i.toString)
        val el = LazyList("default")
        val expected = f(42)
        assert(monadLogic.bindOrElse(a)(f)(el) === expected)
      }

      "returns default value if empty" in {
        val a = monadLogic.empty[Int]
        val f = (i: Int) => LazyList.fill(i)(i.toString)
        val el = LazyList("default")
        val expected = el
        assert(monadLogic.bindOrElse(a)(f)(el) === expected)
      }

      "returns head and bind result of tail" in {
        val head = 1
        val tail = LazyList(2, 3, 4)
        val a = monadLogic.plus(monadLogic.point(head))(tail)
        val f = (i: Int) => LazyList.fill(i)(i.toString)
        val el = LazyList("default")
        val expected = monadLogic.plus(f(head))(monadLogic.bind(tail)(f))
        assert(monadLogic.bindOrElse(a)(f)(el) === expected)
      }
    }
  }
}
