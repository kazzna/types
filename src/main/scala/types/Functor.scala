package types

trait Functor[F[_]] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  final def pair[A](fa: F[A]): F[(A, A)] = map(fa)(a => (a, a))

  final def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())
}

object Functor {
  @inline
  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F

  def compose[F[_], G[_]](F: Functor[F], G: Functor[G]): Functor[[A] =>> F[G[A]]] = new Functor[[A] =>> F[G[A]]] {
    override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = F.map(fga)(G.map(_)(f))
  }
}
