package types

trait Functor[F[_]] { self =>
  def map[A, B]: F[A] => (A => B) => F[B]

  def pair[A]: F[A] => F[(A, A)] = map(_)(a => (a, a))

  def void[A]: F[A] => F[Unit] = map(_)(_ => ())

  def compose[G[_]](G: Functor[G]): Functor[Lambda[A => F[G[A]]]] =
    new Functor[Lambda[A => F[G[A]]]] {
      override def map[A, B]: F[G[A]] => (A => B) => F[G[B]] = fga => f => self.map(fga)(G.map(_)(f))
    }
}

object Functor {
  @inline
  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F
}
