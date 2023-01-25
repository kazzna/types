package types

import scala.annotation.tailrec

trait Plus[F[_]] {
  def plus[A]: F[A] => (=> F[A]) => F[A]

  def unfoldLeftOption[S, A]: S => (S => Option[(S, F[A])]) => Option[F[A]] = { s => f =>
    @tailrec
    def loop(seed: S, acc: F[A]): F[A] = f(seed) match {
      case Some((s, fa)) => loop(s, plus(fa)(acc))
      case None => acc
    }
    f(s).collect { case (s, a) => loop(s, a) }
  }

  def unfoldRightOption[S, A]: S => (S => Option[(F[A], S)]) => Option[F[A]] = { s => f =>
    @tailrec
    def loop(acc: F[A], seed: S): F[A] = f(seed) match {
      case Some((fa, s)) => loop(plus(acc)(fa), s)
      case None => acc
    }

    f(s).collect { case (fa, s) => loop(fa, s) }
  }

  def asSemigroup[A]: Semigroup[F[A]] = new Semigroup[F[A]] {
    override def append: F[A] => (=> F[A]) => F[A] = plus
  }
}

object Plus {
  @inline
  def apply[F[_]](implicit F: Plus[F]): Plus[F] = F
}
