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

  given [Tup <: Tuple: Of[T], T] => (Tuple.Init[Tup] containsOnly T) = refl

  given [Tup <: Tuple: Of[T], N <: Int, T] => (Tuple.Take[Tup, N] containsOnly T) = refl

  given [Tup <: Tuple: Of[T], N <: Int, T] => (Tuple.Drop[Tup, N] containsOnly T) = refl

  given [Tup <: Tuple: Of[T], P[_ <: Tuple.Union[Tup]] <: Boolean, T] => (Tuple.Filter[Tup, P] containsOnly T) = refl

  given [Tup <: Tuple: Of[T], Y <: T, T] => (Tuple.Append[Tup, Y] containsOnly T) = refl

  given [Tup <: Tuple: Of[T], T] => (Tuple.Reverse[Tup] containsOnly T) = refl

  given [Tup1 <: Tuple: Of[T], Tup2 <: Tuple: Of[T], T] => (Tuple.Concat[Tup1, Tup2] containsOnly T) = refl

  given [Tup1 <: Tuple: Of[T1], Tup2 <: Tuple: Of[T2], T1, T2] => (Tuple.Zip[Tup1, Tup2] containsOnly (T1, T2)) = refl

  given [Tup <: Tuple: Of[T], This >: Tup <: Tuple, T] => (Tuple.Head[This] <:< T) =
    <:<.refl.asInstanceOf[(Tuple.Head[This] <:< T)]

  given [Tup <: Tuple: Of[T], This >: Tup <: Tuple, T] => (Tuple.Last[This] <:< T) =
    <:<.refl.asInstanceOf[(Tuple.Last[This] <:< T)]

  given [Tup <: Tuple: Of[T], This >: Tup <: Tuple, T] => (Tuple.Union[This] <:< T) =
    <:<.refl.asInstanceOf[(Tuple.Union[This] <:< T)]

sealed trait containsOnlyLowPriority:
  given [Tup <: Tuple: Of[T], This >: Tup <: Tuple, N <: Int, T] => (Tuple.Elem[This, N] <:< T) =
    <:<.refl.asInstanceOf[(Tuple.Elem[This, N] <:< T)]
  given Tuple containsOnly Any = containsOnly.refl
