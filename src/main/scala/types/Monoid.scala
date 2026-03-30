package types

trait Monoid[A] extends Semigroup[A] {
  def zero: A
}

object Monoid {
  @inline
  def apply[A](implicit A: Monoid[A]): Monoid[A] = A

  def fromSemigroup[A](zero: A, semigroup: Semigroup[A]): Monoid[A] = {
    val zeroValue = zero
    new Monoid[A] {
      override def append(a1: A, a2: => A): A = semigroup.append(a1, a2)
      override def zero: A = zeroValue
    }
  }

  given fromPlusEmpty[F[_], A](using PlusEmpty: PlusEmpty[F]): Monoid[F[A]] = new Monoid[F[A]] {
    override def append(a1: F[A], a2: => F[A]): F[A] = PlusEmpty.plus(a1, a2)
    override def zero: F[A] = PlusEmpty.empty
  }
}
