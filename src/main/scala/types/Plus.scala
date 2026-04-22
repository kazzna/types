package types

import scala.annotation.tailrec

trait Plus[F[_]] {
  def plus[A](fa1: F[A], fa2: => F[A]): F[A]

  final def unfoldLeftOption[S, A](initial: S)(f: S => Option[(S, F[A])]): Option[F[A]] = {
    @tailrec
    def loop(seed: S, acc: F[A]): F[A] = f(seed) match {
      case Some((s, fa)) => loop(s, plus(fa, acc))
      case None => acc
    }
    f(initial).collect { case (s, a) => loop(s, a) }
  }

  def unfoldRightOption[S, A](initial: S)(f: S => Option[(F[A], S)]): Option[F[A]] = {
    @tailrec
    def loop(acc: F[A], seed: S): F[A] = f(seed) match {
      case Some((fa, s)) => loop(plus(acc, fa), s)
      case None => acc
    }

    f(initial).collect { case (fa, s) => loop(fa, s) }
  }
}

object Plus {
  @inline
  def apply[F[_]](implicit F: Plus[F]): Plus[F] = F
}
