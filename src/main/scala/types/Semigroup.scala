package types

trait Semigroup[A] {
  def append: A => (=> A) => A
}

object Semigroup {
  @inline
  def apply[A](implicit A: Semigroup[A]): Semigroup[A] = A
}
