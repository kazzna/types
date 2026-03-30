package types

trait Flatten[F[_]] {
  def flatten[A](ffa: F[F[A]]): F[A]
}

object Flatten {
  @inline
  def apply[F[_]](using F: Flatten[F]): Flatten[F] = F

  given fromBind[F[_]](using F: Bind[F]): Flatten[F] = new Flatten[F] {
    override def flatten[A](ffa: F[F[A]]): F[A] = F.bind(ffa)(identity)
  }
}
