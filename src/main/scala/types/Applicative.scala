package types

trait Applicative[F[_]] extends Functor[F] with Point[F] with Map2[F] { self =>
  override def map[A, B]: F[A] => (A => B) => F[B] = fa => f => ap(fa)(point(f))

  def compose[G[_]](G: Applicative[G]): Applicative[Lambda[A => F[G[A]]]] = new Applicative[Lambda[A => F[G[A]]]] {
    override def point[A]: (=> A) => F[G[A]] = a => self.point(G.point(a))

    override def ap[A, B]: F[G[A]] => F[G[A => B]] => F[G[B]] = fga =>
      fgf => self.ap(fga)(self.map(fgf)(gf => G.ap(_)(gf)))

    override def map2[A, B, C]: F[G[A]] => F[G[B]] => ((A, B) => C) => F[G[C]] =
      Applicative.map2FromPointAndAp[Lambda[A => F[G[A]]], A, B, C](point, ap, ap)
  }

  def map3[A, B, C, D]: F[A] => F[B] => F[C] => ((A, B, C) => D) => F[D] =
    fa => fb => fc => f => ap(fa)(ap(fb)(ap(fc)(point(c => b => a => f(a, b, c)))))

  def map4[A, B, C, D, E]: F[A] => F[B] => F[C] => F[D] => ((A, B, C, D) => E) => F[E] =
    fa => fb => fc => fd => f => ap(fa)(ap(fb)(ap(fc)(ap(fd)(point(d => c => b => a => f(a, b, c, d))))))

  def map5[A, B, C, D, E, G]: F[A] => F[B] => F[C] => F[D] => F[E] => ((A, B, C, D, E) => G) => F[G] = fa =>
    fb =>
      fc => fd => fe => f => ap(fa)(ap(fb)(ap(fc)(ap(fd)(ap(fe)(point(e => d => c => b => a => f(a, b, c, d, e)))))))

  def map6[A, B, C, D, E, G, H]: F[A] => F[B] => F[C] => F[D] => F[E] => F[G] => ((A, B, C, D, E, G) => H) => F[H] =
    fa =>
      fb =>
        fc =>
          fd =>
            fe =>
              fg =>
                f =>
                  ap(fa)(
                    ap(fb)(ap(fc)(ap(fd)(ap(fe)(ap(fg)(point(g => e => d => c => b => a => f(a, b, c, d, e, g)))))))
                  )
}

object Applicative {
  @inline
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  @inline
  def map2FromPointAndAp[F[_], A, B, C](
      point: (=> (B => A => C)) => F[B => A => C],
      ap1: F[A] => F[A => C] => F[C],
      ap2: F[B] => F[B => A => C] => F[A => C]
  ): F[A] => F[B] => ((A, B) => C) => F[C] = fa => fb => f => ap1(fa)(ap2(fb)(point(b => a => f(a, b))))

  @inline
  def apFromMap2[F[_], A, B](
      map2: F[A] => F[A => B] => ((A, A => B) => B) => F[B]
  ): F[A] => F[A => B] => F[B] = Map2.apFromMap2(map2)
}
