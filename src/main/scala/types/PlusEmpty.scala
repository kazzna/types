package types

trait PlusEmpty[F[_]] extends Plus[F] with Empty[F] {
  def unfoldLeft[S, A]: S => (S => Option[(S, F[A])]) => F[A] =
    unfoldLeftOption(_).andThen(_.getOrElse(empty))

  def unfoldRight[S, A]: S => (S => Option[(F[A], S)]) => F[A] =
    unfoldRightOption(_).andThen(_.getOrElse(empty))

  def asMonoid[A]: Monoid[F[A]] = new Monoid[F[A]] {
    override def zero: F[A] = empty

    override def append: F[A] => (=> F[A]) => F[A] = plus
  }
}

object PlusEmpty {
  @inline
  def apply[F[_]](implicit F: PlusEmpty[F]): PlusEmpty[F] = F
}
