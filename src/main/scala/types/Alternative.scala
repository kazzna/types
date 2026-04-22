package types

trait Alternative[F[_]] extends Applicative[F] with PlusEmpty[F] {
  def combine[A](pair: Option[(A, F[A])]): F[A] = pair match {
    case Some((a, fa)) => plus(point(a), fa)
    case None => empty
  }
}

object Alternative {
  @inline
  def apply[F[_]](implicit F: Alternative[F]): Alternative[F] = F

  def from[F[_]](
      applicative: Applicative[F],
      plusEmpty: PlusEmpty[F]
  ): Alternative[F] = new Alternative[F] {
    override def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] = applicative.ap(fa)(ff)

    override def empty[A]: F[A] = plusEmpty.empty

    override def map[A, B](fa: F[A])(f: A => B): F[B] = applicative.map(fa)(f)

    override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = applicative.map2(fa, fb)(f)

    override def plus[A](fa1: F[A], fa2: => F[A]): F[A] = plusEmpty.plus(fa1, fa2)

    override def point[A](a: => A): F[A] = applicative.point(a)
  }
}
