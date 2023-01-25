package types

trait Alternative[F[_]] extends Applicative[F] with PlusEmpty[F] {
  def combine[A]: Option[(A, F[A])] => F[A] = {
    case Some((a, fa)) => plus(point(a))(fa)
    case None => empty
  }
}

object Alternative {
  @inline
  def apply[F[_]](implicit F: Alternative[F]): Alternative[F] = F

  @inline
  def map2FromPointAndAp[F[_], A, B, C](
      point: (=> (B => A => C)) => F[B => A => C],
      ap1: F[A] => F[A => C] => F[C],
      ap2: F[B] => F[B => A => C] => F[A => C]
  ): F[A] => F[B] => ((A, B) => C) => F[C] = Applicative.map2FromPointAndAp(point, ap1, ap2)

  @inline
  def apFromMap2[F[_], A, B](
      map2: F[A] => F[A => B] => ((A, A => B) => B) => F[B]
  ): F[A] => F[A => B] => F[B] = Applicative.apFromMap2(map2)
}
