package types

trait MonadPlus[F[_]] extends Monad[F] with Alternative[F] {
  def filter[A]: F[A] => (A => Boolean) => F[A] = fa => f => bind(fa)(a => if (f(a)) point(a) else empty)
}

object MonadPlus {
  @inline
  def apply[F[_]](implicit F: MonadPlus[F]): MonadPlus[F] = F
}
