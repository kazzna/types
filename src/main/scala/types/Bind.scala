package types

trait Bind[F[_]] {
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object Bind {
  @inline
  def apply[F[_]](implicit F: Bind[F]): Bind[F] = F
}
