package types

trait Group[A] extends Monoid[A] {
  def inverse(a: A): A
}

object Group {
  @inline
  def apply[A](implicit A: Group[A]): Group[A] = A

  def fromMonoid[A](inverse: A => A, monoid: Monoid[A]): Group[A] = {
    val inverseFun = inverse
    new Group[A] {
      override def append(a1: A, a2: => A): A = monoid.append(a1, a2)

      override def inverse(a: A): A = inverseFun(a)

      override def zero: A = monoid.zero
    }
  }
}
