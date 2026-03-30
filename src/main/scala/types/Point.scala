package types

trait Point[F[_]] { self =>
  def point[A](a: => A): F[A]
}

object Point {
  @inline
  def apply[F[_]](implicit F: Point[F]): Point[F] = F

  def compose[F[_], G[_]](F: Point[F], G: Point[G]): Point[[A] =>> F[G[A]]] = new Point[[A] =>> F[G[A]]] {
    override def point[A](a: => A): F[G[A]] = F.point(G.point(a))
  }
}
