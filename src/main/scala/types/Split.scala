package types

trait Split[F[_]] {
  def split[A](fa: F[A]): F[Option[(A, F[A])]]
}
