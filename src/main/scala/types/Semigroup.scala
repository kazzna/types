package types

trait Semigroup[A] {
  def append(a1: A, a2: => A): A
}

object Semigroup {
  @inline
  def apply[A](implicit A: Semigroup[A]): Semigroup[A] = A

  given fromPlus[F[_], A](using plus: Plus[F]): Semigroup[F[A]] = new Semigroup[F[A]] {
    override def append(a1: F[A], a2: => F[A]): F[A] = plus.plus(a1, a2)
  }
}
