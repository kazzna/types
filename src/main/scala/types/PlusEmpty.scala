package types

trait PlusEmpty[F[_]] extends Plus[F] with Empty[F] {
  final def unfoldLeft[S, A](initial: S)(f: S => Option[(S, F[A])]): F[A] =
    unfoldLeftOption(initial)(f).getOrElse(empty)

  final def unfoldRight[S, A](initial: S)(f: S => Option[(F[A], S)]): F[A] =
    unfoldRightOption(initial)(f).getOrElse(empty)

  /*
  def asMonoid[A]: Monoid[F[A]] = new Monoid[F[A]] {
    override def zero: F[A] = empty

    override def append: F[A] => (=> F[A]) => F[A] = plus
  }
   */
}

object PlusEmpty {
  @inline
  def apply[F[_]](implicit F: PlusEmpty[F]): PlusEmpty[F] = F

  def from[F[_]](
      plus: Plus[F],
      empty: Empty[F]
  ): PlusEmpty[F] = {
    val plusType = plus
    val emptyType = empty
    new PlusEmpty[F] {
      override def empty[A]: F[A] = emptyType.empty

      override def plus[A](fa1: F[A], fa2: => F[A]): F[A] = plusType.plus(fa1, fa2)
    }
  }
}
