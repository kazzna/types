package types

trait Empty[F[_]] {
  def empty[A]: F[A]
}

object Empty {
  @inline
  def apply[F[_]](implicit F: Empty[F]): Empty[F] = F
}
