package types

trait Map2[F[_]] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
}

object Map2 {
  @inline
  def apply[F[_]](implicit F: Map2[F]): Map2[F] = F

  def compose[F[_], G[_]](F: Map2[F], G: Map2[G]): Map2[[A] =>> F[G[A]]] = new Map2[[A] =>> F[G[A]]] {
    override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
      F.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
  }

  given fromApplicative[F[_]](using F: Applicative[F]): Map2[F] = new Map2[F] {
    override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = F.map2(fa, fb)(f)
  }
}
