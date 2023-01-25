package types

trait Monoid[A] extends Semigroup[A] {
  def zero: A
}

object Monoid {
  @inline
  def apply[A](implicit A: Monoid[A]): Monoid[A] = A
}
