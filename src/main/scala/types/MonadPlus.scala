package types

trait MonadPlus[F[_]] extends Monad[F] with PlusEmpty[F] {
  def filter[A](fa: F[A])(f: A => Boolean): F[A] = bind(fa)(a => if (f(a)) point(a) else empty)
}

object MonadPlus {
  @inline
  def apply[F[_]](implicit F: MonadPlus[F]): MonadPlus[F] = F

  def from[F[_]](
      Monad: Monad[F],
      PlusEmpty: PlusEmpty[F]
  ): MonadPlus[F] = new MonadPlus[F] {
    override def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] = Monad.ap(fa)(ff)

    override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = Monad.bind(fa)(f)

    override def empty[A]: F[A] = PlusEmpty.empty

    override def flatten[A](ffa: F[F[A]]): F[A] = Monad.flatten(ffa)

    override def map[A, B](fa: F[A])(f: A => B): F[B] = Monad.map(fa)(f)

    override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = Monad.map2(fa, fb)(f)

    override def plus[A](fa1: F[A], fa2: => F[A]): F[A] = PlusEmpty.plus(fa1, fa2)

    override def point[A](a: => A): F[A] = Monad.point(a)
  }

  def fromAlternative[F[_]](
      Alternative: Alternative[F],
      Bind: Bind[F]
  ): MonadPlus[F] = new MonadPlus[F] {
    override def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] = Alternative.ap(fa)(ff)

    override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = Bind.bind(fa)(f)

    override def empty[A]: F[A] = Alternative.empty

    override def flatten[A](ffa: F[F[A]]): F[A] = bind(ffa)(identity)

    override def map[A, B](fa: F[A])(f: A => B): F[B] = Alternative.map(fa)(f)

    override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = Alternative.map2(fa, fb)(f)

    override def plus[A](fa1: F[A], fa2: => F[A]): F[A] = Alternative.plus(fa1, fa2)

    override def point[A](a: => A): F[A] = Alternative.point(a)
  }
}
