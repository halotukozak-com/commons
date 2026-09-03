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

  inline given [Tup <: Tuple, T] => (Loop[Tup, T] =:= true) => containsOnly[Tup, T] = refl

  /** A constant map `[_] =>> C` makes every element `C`. Unifies even for abstract `Es`. */
  inline given [Es <: Tuple, C] => (Tuple.Map[Es, [_] =>> C] containsOnly C) = refl

  /** A covariant `F` gives `F[e] <: F[Any]` for every element (invariant `F` still needs `refl`). */
  inline given [Es <: Tuple, F[+_]] => (Tuple.Map[Es, F] containsOnly F[Any]) = refl

  inline given [Tup <: Tuple, T](using inline ev: Tup containsOnly T): (Tuple.Tail[Tup] containsOnly T) = refl
  inline given [Tup <: Tuple, T](using inline ev: Tup containsOnly T): (Tuple.Reverse[Tup] containsOnly T) = refl
  inline given [Tup1 <: Tuple, Tup2 <: Tuple, T](
    using inline ev1: Tup1 containsOnly T,
    ev2: Tup2 containsOnly T,
  ): (Tuple.Concat[Tup1, Tup2] containsOnly T) = refl
  inline given [Tup1 <: Tuple, Tup2 <: Tuple, T1, T2](
    using inline ev1: Tup1 containsOnly T1,
    ev2: Tup2 containsOnly T2,
  ): (Tuple.Zip[Tup1, Tup2] containsOnly (T1, T2)) = refl

  import scala.language.implicitConversions

  inline def headConv[Tup <: Tuple, T](
    inline head: Tuple.Head[Tup],
  )(using inline ev: Tup containsOnly T,
  ): T = head.asInstanceOf[T]
  inline def lastConv[Tup <: Tuple, T](
    inline last: Tuple.Last[Tup],
  )(using inline ev: Tup containsOnly T,
  ): T = last.asInstanceOf[T]

sealed trait containsOnlyLowPriority:
  inline given Tuple containsOnly Any = containsOnly.refl
