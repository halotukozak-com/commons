package halotukozak.commons

type Of[T] = [Tup <: Tuple] =>> Tup containsOnly T

infix sealed trait containsOnly[-Tup <: Tuple, +T]

object containsOnly extends containsOnlyLowPriority:

  type Loop[Tup <: Tuple, T] <: Boolean = Tup match
    case EmptyTuple => true
    case T *: tail => Loop[tail, T]
    case _ => false

  private val reusable = new containsOnly[Tuple, Nothing] {}

  def refl[Tup <: Tuple, T]: Tup containsOnly T = reusable

  given [Tup <: Tuple, T](using Loop[Tup, T] =:= true): containsOnly[Tup, T] = refl

  /** A constant map `[_] =>> C` makes every element `C`. Unifies even for abstract `Es`. */
  given [Es <: Tuple, C] => (Tuple.Map[Es, [_] =>> C] containsOnly C) = refl

  /** A covariant `F` gives `F[e] <: F[Any]` for every element (invariant `F` still needs `refl`). */
  given [Es <: Tuple, F[+_]] => (Tuple.Map[Es, F] containsOnly F[Any]) = refl

  given [Tup <: Tuple, T](using ev: Tup containsOnly T): (Tuple.Tail[Tup] containsOnly T) = refl

  given [Tup <: Tuple, T](using ev: Tup containsOnly T): (Tuple.Reverse[Tup] containsOnly T) = refl

  given [Tup1 <: Tuple, Tup2 <: Tuple, T](
    using ev1: Tup1 containsOnly T,
    ev2: Tup2 containsOnly T,
  ): (Tuple.Concat[Tup1, Tup2] containsOnly T) = refl

  given [Tup1 <: Tuple, Tup2 <: Tuple, T1, T2](
    using ev1: Tup1 containsOnly T1,
    ev2: Tup2 containsOnly T2,
  ): (Tuple.Zip[Tup1, Tup2] containsOnly (T1, T2)) = refl


  given [Tup <: Tuple, This >: Tup <: Tuple, T](using ev: Tup containsOnly T): (Tuple.Head[This] <:< T) = <:<.refl.asInstanceOf[(Tuple.Head[This] <:< T)]

  given [Tup <: Tuple, This >: Tup <: Tuple, T](using ev: Tup containsOnly T): (Tuple.Last[This] <:< T) = <:<.refl.asInstanceOf[(Tuple.Last[This] <:< T)]

sealed trait containsOnlyLowPriority:
  given Tuple containsOnly Any = containsOnly.refl
