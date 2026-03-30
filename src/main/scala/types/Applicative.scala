package types

trait Applicative[F[_]] extends Functor[F] with Point[F] with Apply[F] with Map2[F] { self =>
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    ap(fa)(ap(fb)(ap(fc)(point(c => b => a => f(a, b, c)))))

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    ap(fa)(ap(fb)(ap(fc)(ap(fd)(point(d => c => b => a => f(a, b, c, d))))))

  def map5[A, B, C, D, E, G](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])(f: (A, B, C, D, E) => G): F[G] =
    ap(fa)(ap(fb)(ap(fc)(ap(fd)(ap(fe)(point(e => d => c => b => a => f(a, b, c, d, e)))))))

  def map6[A, B, C, D, E, G, H](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G])(
      f: (A, B, C, D, E, G) => H
  ): F[H] =
    ap(fa)(ap(fb)(ap(fc)(ap(fd)(ap(fe)(ap(fg)(point(g => e => d => c => b => a => f(a, b, c, d, e, g))))))))
}

object Applicative {
  @inline
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  def compose[F[_], G[_]](f: Applicative[F], g: Applicative[G]): Applicative[[A] =>> F[G[A]]] = fromMap2(
    Functor.compose(f, g),
    Point.compose(f, g),
    Map2.compose(Map2.fromApplicative(using f), Map2.fromApplicative(using g))
  )

  def fromApply[F[_]](
      functor: Functor[F],
      point: Point[F],
      apply: Apply[F]
  ): Applicative[F] = {
    val pointType = point
    new Applicative[F] {
      override def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] = apply.ap(fa)(ff)

      override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
        ap(fa)(ap(fb)(point(b => a => f(a, b))))

      override def map[A, B](fa: F[A])(f: A => B): F[B] = functor.map(fa)(f)

      override def point[A](a: => A): F[A] = pointType.point(a)
    }
  }

  def fromMap2[F[_]](
      functor: Functor[F],
      point: Point[F],
      map2: Map2[F]
  ): Applicative[F] = {
    val pointType = point
    val map2Type = map2
    new Applicative[F] {
      override def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] =
        map2(fa, ff)((a, f) => f(a))

      override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = map2Type.map2(fa, fb)(f)

      override def map[A, B](fa: F[A])(f: A => B): F[B] = functor.map(fa)(f)

      override def point[A](a: => A): F[A] = pointType.point(a)
    }
  }
}
