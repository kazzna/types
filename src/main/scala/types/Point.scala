package types

trait Point[F[_]] { self =>
  def point[A]: (=> A) => F[A]

  def compose[G[_]](G: Point[G]): Point[Lambda[A => F[G[A]]]] =
    new Point[Lambda[A => F[G[A]]]] {
      override def point[A]: (=> A) => F[G[A]] = a => self.point(G.point(a))
    }
}

object Point {
  @inline
  def apply[F[_]](implicit F: Point[F]): Point[F] = F
}
