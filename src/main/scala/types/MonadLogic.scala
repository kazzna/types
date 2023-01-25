package types

trait MonadLogic[F[_]] extends MonadPlus[F] {
  def split[A]: F[A] => F[Option[(A, F[A])]]

  def interleave[A]: F[A] => (=> F[A]) => F[A] = { fa => fb =>
    val mapping = (a: A, fa: F[A]) => plus(point(a))(interleave(fb)(fa))
    bind(split(fa))(_.map(mapping.tupled).getOrElse(fb))
  }

  def bindLike[A, B]: F[A] => (A => F[B]) => F[B] = fa =>
    f => splitThenBindOrElse(fa)((a, fa) => interleave(f(a))(bindLike(fa)(f)))(empty)

  def once[A]: F[A] => F[A] = fa => splitThenBindOrElse(fa)((a, _) => point(a))(empty)

  def not[A]: F[A] => F[Unit] = fa => splitThenBindOrElse(fa)((_, _) => empty[Unit])(point(()))

  def bindOrElse[A, B]: F[A] => (A => F[B]) => (=> F[B]) => F[B] =
    fa => f => fb => splitThenBindOrElse(fa)((a, fa) => plus(f(a))(bind(fa)(f)))(fb)

  @inline
  private def splitThenBindOrElse[A, B](fa: F[A])(f: (A, F[A]) => F[B])(fb: => F[B]): F[B] =
    bind(split(fa))(_.map(f.tupled).getOrElse(fb))
}

object MonadLogic {
  @inline
  def apply[F[_]](implicit F: MonadLogic[F]): MonadLogic[F] = F
}
