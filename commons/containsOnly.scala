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

  given [Tup <: Tuple, T] => (Loop[Tup, T] =:= true) => (Tup containsOnly T) = refl

  /** A constant map `[_] =>> C` makes every element `C`. Unifies even for abstract `Es`. */
  given [Es <: Tuple, C] => (Tuple.Map[Es, [_] =>> C] containsOnly C) = refl

  /** A covariant `F` gives `F[e] <: F[Any]` for every element (invariant `F` still needs `refl`). */
  given [Es <: Tuple, F[+_]] => (Tuple.Map[Es, F] containsOnly F[Any]) = refl

  given [Tup <: Tuple: Of[T], T] => (Tuple.Tail[Tup] containsOnly T) = refl

  given [Tup <: Tuple: Of[T], T] => (Tuple.Reverse[Tup] containsOnly T) = refl

  given [Tup1 <: Tuple: Of[T], Tup2 <: Tuple: Of[T], T] => (Tuple.Concat[Tup1, Tup2] containsOnly T) = refl

  given [Tup1 <: Tuple: Of[T1], Tup2 <: Tuple: Of[T2], T1, T2] => (Tuple.Zip[Tup1, Tup2] containsOnly (T1, T2)) = refl

  given [Tup <: Tuple: Of[T], This >: Tup <: Tuple, T] => (Tuple.Head[This] <:< T) =
    <:<.refl.asInstanceOf[(Tuple.Head[This] <:< T)]

  given [Tup <: Tuple: Of[T], This >: Tup <: Tuple, T] => (Tuple.Last[This] <:< T) =
    <:<.refl.asInstanceOf[(Tuple.Last[This] <:< T)]

sealed trait containsOnlyLowPriority:
  given Tuple containsOnly Any = containsOnly.refl
