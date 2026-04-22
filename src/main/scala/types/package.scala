package object types {
  /**
   * Type alias for natural transformations using infix notation.
   *
   * This alias allows natural transformations from type constructor `F` to type constructor `G`
   * to be written in infix form as `F ~> G`, which is more readable and intuitive than
   * `NaturalTransformation[F, G]`. The symbolic notation `~>` is inspired by category-theoretic
   * conventions and is commonly used in the Scala type class ecosystem.
   *
   * @tparam F The source type constructor
   * @tparam G The target type constructor
   *
   * @example
   * {{
   *   val optionToList: Option ~> List = new NaturalTransformation[Option, List] {
   *     override def apply[A](fa: Option[A]): List[A] = fa.toList
   *   }
   * }}
   */
  type ~>[F[_], G[_]] = NaturalTransformation[F, G]
}
