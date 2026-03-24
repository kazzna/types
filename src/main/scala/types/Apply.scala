package types

trait Apply[F[_]] { self =>
  def ap[A, B](fa: F[A])(ff: F[A => B]): F[B]
}

object Apply {
  @inline
  def apply[F[_]](using F: Apply[F]): Apply[F] = F

  given fromMap2[F[_]](using F: Map2[F]): Apply[F] = new Apply[F] {
    override def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] = F.map2(fa, ff)((a, f) => f(a))
  }

  given fromApplicative[F[_]](using F: Applicative[F]): Apply[F] = new Apply[F] {
    override def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] = F.ap(fa)(ff)
  }
}
