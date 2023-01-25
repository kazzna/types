package types

trait Monad[F[_]] extends Applicative[F] with Bind[F] {
  override def ap[A, B]: F[A] => F[A => B] => F[B] = fa => ff => bind(fa)(a => bind(ff)(f => point(f(a))))

  override def map2[A, B, C]: F[A] => F[B] => ((A, B) => C) => F[C] = fa =>
    fb => f => bind(fa)(a => bind(fb)(b => point(f(a, b))))
}

object Monad {
  @inline
  def apply[F[_]](implicit F: Monad[F]): Monad[F] = F
}
