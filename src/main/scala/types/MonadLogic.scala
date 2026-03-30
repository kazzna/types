package types

trait MonadLogic[F[_]] extends MonadPlus[F] with Split[F] {
  def split[A](fa: F[A]): F[Option[(A, F[A])]]

  def interleave[A](fa1: F[A], fa2: => F[A]): F[A] = {
    val mapping = (a: A, fa: F[A]) => plus(point(a), interleave(fa2, fa))
    bind(split(fa1))(_.map(mapping.tupled).getOrElse(fa2))
  }

  def bindLike[A, B](fa: F[A])(f: A => F[B]): F[B] =
    splitThenBindOrElse(fa)((a, fa) => interleave(f(a), bindLike(fa)(f)))(empty)

  def once[A](fa: F[A]): F[A] = splitThenBindOrElse(fa)((a, _) => point(a))(empty)

  def not[A](fa: F[A]): F[Unit] = splitThenBindOrElse(fa)((_, _) => empty[Unit])(point(()))

  def bindOrElse[A, B](fa: F[A])(f: A => F[B])(fb: => F[B]): F[B] =
    splitThenBindOrElse(fa)((a, fa) => plus(f(a), bind(fa)(f)))(fb)

  @inline
  private def splitThenBindOrElse[A, B](fa: F[A])(f: (A, F[A]) => F[B])(fb: => F[B]): F[B] =
    bind(split(fa))(_.map(f.tupled).getOrElse(fb))
}

object MonadLogic {
  @inline
  def apply[F[_]](implicit F: MonadLogic[F]): MonadLogic[F] = F

  def from[F[_]](
      MonadPlus: MonadPlus[F],
      Split: Split[F]
  ): MonadLogic[F] = new MonadLogic[F] {
    override def split[A](fa: F[A]): F[Option[(A, F[A])]] = Split.split(fa)

    override def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] = MonadPlus.ap(fa)(ff)

    override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = MonadPlus.bind(fa)(f)

    override def empty[A]: F[A] = MonadPlus.empty

    override def flatten[A](ffa: F[F[A]]): F[A] = MonadPlus.flatten(ffa)

    override def map[A, B](fa: F[A])(f: A => B): F[B] = MonadPlus.map(fa)(f)

    override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = MonadPlus.map2(fa, fb)(f)

    override def plus[A](fa1: F[A], fa2: => F[A]): F[A] = MonadPlus.plus(fa1, fa2)

    override def point[A](a: => A): F[A] = MonadPlus.point(a)
  }
}
