package halotukozak.commons

import scala.collection.Factory
import scala.util.control.TailCalls.{done, tailcall, TailRec}

trait TailRecTraversable[F[_]]:
  def traverse[A, B](x: F[A])(f: A => TailRec[B]): TailRec[F[B]]

object TailRecTraversable:

  private def traverseIterable[A, B, C](xs: IterableOnce[A])(f: A => TailRec[B])(using factory: Factory[B, C])
    : TailRec[C] =
    def go(it: Iterator[A]): TailRec[List[B]] =
      if it.hasNext then tailcall(f(it.next())).flatMap(b => tailcall(go(it)).map(b :: _)) else done(Nil)

    go(xs.iterator).map(factory.fromSpecific)

  given TailRecTraversable[Option]:
    def traverse[A, B](x: Option[A])(f: A => TailRec[B]): TailRec[Option[B]] = x match
      case None => done(None)
      case Some(a) => tailcall(f(a)).map(Some(_))

  given [L] => TailRecTraversable[[X] =>> Either[L, X]]:
    def traverse[A, B](x: Either[L, A])(f: A => TailRec[B]): TailRec[Either[L, B]] = x match
      case Left(l) => done(Left(l))
      case Right(a) => tailcall(f(a)).map(Right(_))

  given TailRecTraversable[List]:
    def traverse[A, B](x: List[A])(f: A => TailRec[B]): TailRec[List[B]] = traverseIterable(x)(f)

  given TailRecTraversable[Vector]:
    def traverse[A, B](x: Vector[A])(f: A => TailRec[B]): TailRec[Vector[B]] = traverseIterable(x)(f)

  given TailRecTraversable[Set]:
    def traverse[A, B](x: Set[A])(f: A => TailRec[B]): TailRec[Set[B]] = traverseIterable(x)(f)
