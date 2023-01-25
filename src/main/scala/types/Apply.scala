package types

trait Apply[F[_]] { self =>
  def ap[A, B]: F[A] => F[A => B] => F[B]
}

object Apply {
  @inline
  def apply[F[_]](implicit F: Apply[F]): Apply[F] = F
}
