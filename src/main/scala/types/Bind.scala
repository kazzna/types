package types

trait Bind[F[_]] {
  def bind[A, B]: F[A] => (A => F[B]) => F[B]

  def flatten[A]: F[F[A]] => F[A] = bind(_)(identity)
}

object Bind {
  @inline
  def apply[F[_]](implicit F: Bind[F]): Bind[F] = F
}
