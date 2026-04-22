package types

trait Monad[F[_]] extends Applicative[F] with Bind[F] with Flatten[F]

object Monad {
  @inline
  def apply[F[_]](implicit F: Monad[F]): Monad[F] = F

  def fromBind[F[_]](
      applicative: Applicative[F],
      bind: Bind[F]
  ): Monad[F] = {
    val bindType = bind
    new Monad[F] {
      override def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] = applicative.ap(fa)(ff)

      override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = bindType.bind(fa)(f)

      override def flatten[A](ffa: F[F[A]]): F[A] = bind(ffa)(identity)

      override def map[A, B](fa: F[A])(f: A => B): F[B] = applicative.map(fa)(f)

      override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = applicative.map2(fa, fb)(f)

      override def point[A](a: => A): F[A] = applicative.point(a)
    }
  }

  def fromFlatten[F[_]](
      applicative: Applicative[F],
      flatten: Flatten[F]
  ): Monad[F] = {
    val flattenType = flatten
    new Monad[F] {
      override def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] = applicative.ap(fa)(ff)

      override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = flatten(map(fa)(f))

      override def flatten[A](ffa: F[F[A]]): F[A] = flattenType.flatten(ffa)

      override def map[A, B](fa: F[A])(f: A => B): F[B] = applicative.map(fa)(f)

      override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = applicative.map2(fa, fb)(f)

      override def point[A](a: => A): F[A] = applicative.point(a)
    }
  }

  def fromPoint[F[_]](
      point: Point[F],
      bind: Bind[F]
  ): Monad[F] = {
    val pointType = point
    val bindType = bind
    new Monad[F] {
      override def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] = bind(fa)(a => map(ff)(f => f(a)))

      override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = bindType.bind(fa)(f)

      override def flatten[A](ffa: F[F[A]]): F[A] = bind(ffa)(identity)

      override def map[A, B](fa: F[A])(f: A => B): F[B] = bind(fa)(a => point(f(a)))

      override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = bind(fa)(a => map(fb)(b => f(a, b)))

      override def point[A](a: => A): F[A] = pointType.point(a)
    }
  }
}
