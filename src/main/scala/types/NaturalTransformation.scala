package types

import scala.annotation.tailrec

/**
 * Represents a natural transformation (morphism) between two type constructors.
 *
 * A natural transformation from `F` to `G` provides a way to convert any value of type `F[A]`
 * into a value of type `G[A]` in a manner that is uniform across all types `A`. This trait
 * encapsulates the essence of a natural transformation in category theory.
 *
 * @tparam F The source type constructor
 * @tparam G The target type constructor
 */
trait NaturalTransformation[F[_], G[_]] {
  /**
   * Applies this natural transformation to a value of type `F[A]`, producing a value of type `G[A]`.
   *
   * This method must be implemented for each specific natural transformation, and its implementation
   * must be polymorphic across all types `A`. The transformation shall be applied uniformly
   * regardless of the type `A` in question.
   *
   * @tparam A The type to be transformed
   * @param fa A value of type `F[A]` to be transformed
   * @return A value of type `G[A]`, the result of applying this natural transformation
   */
  def apply[A](fa: F[A]): G[A]

  /**
   * Composes this natural transformation with another such that their effects are applied sequentially.
   *
   * Given this transformation `F ~> G` and another transformation `G ~> H`, this method produces
   * a new transformation `F ~> H` that applies this transformation first, then applies the provided
   * transformation to the result. This represents the canonical left-to-right composition of transformations.
   *
   * @tparam H The target type constructor of the provided transformation
   * @param that A natural transformation from `G` to `H`
   * @return A new natural transformation from `F` to `H` representing the composed effect
   */
  final def andThen[H[_]](that: G ~> H): F ~> H = NaturalTransformation.Sequence(this, that)

  /**
   * Composes this natural transformation with another in reverse order.
   *
   * Given this transformation `G ~> H` and another transformation `F ~> G`, this method produces
   * a new transformation `F ~> H` that applies the provided transformation first, then applies this
   * transformation to the result. This method facilitates the right-to-left composition of transformations,
   * which may be more natural in certain contexts.
   *
   * @tparam E The source type constructor of the provided transformation
   * @param that A natural transformation from `E` to `F`
   * @return A new natural transformation from `E` to `G` representing the composed effect
   */
  final def compose[E[_]](that: E ~> F): E ~> G = NaturalTransformation.Sequence(that, this)

  /**
   * Symbolic alias for the `andThen` method, providing a compact notation for left-to-right composition.
   *
   * This method is equivalent to `andThen` and allows one to write natural transformation composition
   * in a more concise manner using the operator `>>>`, which intuitively represents forward chaining.
   *
   * @tparam H The target type constructor of the provided transformation
   * @param that A natural transformation from `G` to `H`
   * @return A new natural transformation from `F` to `H` representing the composed effect
   */
  @inline
  final def >>>[H[_]](that: G ~> H): F ~> H = andThen(that)

  /**
   * Symbolic alias for the `compose` method, providing a compact notation for right-to-left composition.
   *
   * This method is equivalent to `compose` and allows one to write natural transformation composition
   * in a more concise manner using the operator `<<<`, which intuitively represents backward chaining.
   *
   * @tparam E The source type constructor of the provided transformation
   * @param that A natural transformation from `E` to `F`
   * @return A new natural transformation from `E` to `G` representing the composed effect
   */
  @inline
  final def <<<[E[_]](that: E ~> F): E ~> G = compose(that)
}

object NaturalTransformation {
  /**
   * Internal case class representing the sequential composition of two natural transformations.
   *
   * This class implements the composition logic by deferring evaluation and utilising tail recursion
   * to optimise the application of multiple composed transformations. It ensures that even deeply
   * nested compositions can be applied efficiently without risk of stack overflow.
   *
   * @tparam F The source type constructor
   * @tparam G The intermediate type constructor
   * @tparam H The target type constructor
   * @param f The first natural transformation to apply
   * @param g The second natural transformation to apply
   */
  private case class Sequence[F[_], G[_], H[_]](f: F ~> G, g: G ~> H) extends NaturalTransformation[F, H] {
    override final def apply[A](fa: F[A]): H[A] = {
      @tailrec
      def loop[I[_]](ia: I[A], nt: NaturalTransformation[I, H]): H[A] = nt match {
        case Sequence(f, g) => f match {
          case Sequence(ff, fg) => loop(ia, ff.andThen(fg.andThen(g)))
          case _ => loop(f(ia), g)
        }
        case _ => nt(ia)
      }

      loop(fa, this)
    }
  }
}
