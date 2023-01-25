package types

trait Map2[F[_]] extends Apply[F] {
  def map2[A, B, C]: F[A] => F[B] => ((A, B) => C) => F[C]
}

object Map2 {
  @inline
  def apply[F[_]](implicit F: Map2[F]): Map2[F] = F

  def apFromMap2[F[_], A, B](
      map2: F[A] => F[A => B] => ((A, A => B) => B) => F[B]
  ): F[A] => F[A => B] => F[B] = fa => map2(fa)(_)((a, f) => f(a))
}
