package types

trait Group[A] extends Monoid[A] {
  def inverse: A => A
}

object Group {
  @inline
  def apply[A](implicit A: Group[A]): Group[A] = A
}
