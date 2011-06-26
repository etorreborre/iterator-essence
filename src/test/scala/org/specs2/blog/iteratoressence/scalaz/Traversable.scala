package org.specs2.blog
package iteratoressence
package scalaz

import State._
import Applicative._

import Monad._

trait Traversable[T[_]] {

  def traverse[F[_] : Applicative, A, B](f: A => F[B]): T[A] => F[T[B]]

  def traverseState[A, B, S](f: A => State[S, B]) = traverse[({type l[U]=State[S, U]})#l, A, B](f)

  def traverseMonad[A, B, M[_] : Monad](f: A => M[B]) = traverse(f)

  def reduceConst[A, M : Monoid](m: M): T[A] => M = reduce((a: A) => m)

  def reduce[A, M : Monoid](reducer: A => M): T[A] => M = {
    val f = (a: A) => Const[M, Any](reducer(a))
    (ta: T[A]) => traverse[({type l[A]=Const[M, A]})#l, A, Any](f).apply(ta).value
  }

  def contents[A]: T[A] => List[A] = reduce((a: A) => List(a))

  def count[A]: T[A] => Int = reduceConst(1)

  def map[A, B](mapper: A => B) = (ta: T[A]) => traverse((a: A) => Ident(mapper(a))).apply(ta).value
  def shape[A] = map((a: A) => ())

  def decompose[A] = {

    val shape   = (a: A) => Ident(())
    val content = (a: A) => Const[List[A], Unit](List(a))
    val product = (a: A) => (shape(a).⊗[({type l[T] = Const[List[A], T]})#l](content(a)))

    (ta: T[A]) => {
      val (Ident(s), Const(c)) = traverse[({type l[V] = Product[Ident, ({type l1[U] = Const[List[A], U]})#l1, V]})#l, A, Unit](product).apply(ta).tuple
      (s, c)
    }
  }

  def collect[F[_] : Applicative, A, B](f: A => F[Unit], g: A => B) = {
    val applicative = implicitly[Applicative[F]]
    import applicative._

    val application = (a: A) => point((u: Unit) => g(a)) <*> f(a)
    traverse(application)
  }

  def disperse[F[_] : Applicative, A, B, C](f: F[B], g: A => B => C) = {
    val applicative = implicitly[Applicative[F]]
    import applicative._

    val application = (a: A) => point(g(a)) <*> f
    traverse(application)
  }

  def measure[F[_] : Applicative, A, B](f: F[Unit], g: A => B) = {
    val applicative = implicitly[Applicative[F]]
    import applicative._

    val application = (a: A) => point((u: Unit) => g(a)) <*> f
    traverse(application)
  }

  def measureState[S, A, B](f: State[S, Unit], g: A => B) = {
    measure[({type l[A]=State[S, A]})#l, A, B](f, g)
  }

  def assemble[A, B](elements: List[B])(implicit ev: A =:= Unit) = {

    implicit val optionalStateApplicative = ComposedIsApplicative[({type l[U]=State[List[B], U]})#l, Option]

    def takeHead: State[List[B], Option[B]] = state { s: List[B] =>
      s match {
        case Nil     => (Nil, None)
        case x :: xs => (xs, Some(x))
      }
    }
    (ta: T[A]) =>
     traverse[({type l1[V]=({type l[U]=State[List[B], U]})#l[Option[V]]})#l1, A, B]((a: A) => takeHead).apply(ta).apply(elements)
  }

}

object Traversable {

  implicit def BinaryTreeIsTraversable[A]: Traversable[BinaryTree] = new Traversable[BinaryTree] {

    def createLeaf[B] = (n: B) => (Leaf(n): (BinaryTree[B]))
    def createBin[B]  = (nl: BinaryTree[B]) => (nr: BinaryTree[B]) => (Bin(nl, nr): BinaryTree[B])

    def traverse[F[_] : Applicative, A, B](f: A => F[B]): BinaryTree[A] => F[BinaryTree[B]] = (t: BinaryTree[A]) => {
      t match {
        case Leaf(a)   => createLeaf[B] ∘ f(a)
        case Bin(l, r) => createBin[B]  ∘ (l traverse f) <*> (r traverse f)
      }
    }
  }

  implicit def ListIsTraversable[A]: Traversable[List] = new Traversable[List] {

    def traverse[F[_] : Applicative, A, B](f: A => F[B]): List[A] => F[List[B]] = (l: List[A]) => {
      val applicative = implicitly[Applicative[F]]
      l match {
        case Nil       => applicative.point(List[B]())
        case a :: rest => ((_:B) :: (_: List[B])).curried ∘ f(a) <*> (rest traverse f)
      }
    }

  }

  implicit def TraversableSyntax[A, T[_] : Traversable](t: T[A]): TraversableSyntax[A, T] = new TraversableSyntax[A, T](t)

  class TraversableSyntax[A, T[_] : Traversable](t: T[A]) {
    val tt = implicitly[Traversable[T]]

    def traverse[B, F[_] : Applicative](f: A => F[B]) = tt.traverse(f).apply(t)
    def traverseState[B, S](f: A => State[S, B]) = tt.traverseState(f).apply(t)
    def traverseMonad[B, M[_] : Monad](f: A => M[B]) = tt.traverseMonad(f).apply(t)
    def reduceMonoid[M : Monoid](f: A => M) = tt.reduce(f).apply(t)
    def reduce[M : Monoid](f: A => M) = tt.reduce(f).apply(t)
    def contents: List[A] = tt.contents.apply(t)
    def count: Int = tt.count.apply(t)
    def map[B](f: A => B) = tt.map(f).apply(t)
    def shape = tt.shape.apply(t)
    def decompose = tt.decompose.apply(t)
    def collect[F[_] : Applicative, B](f: A => F[Unit], g: A => B) = tt.collect(f, g).apply(t)
    def disperse[F[_] : Applicative, B, C](f: F[B], g: A => B => C) = tt.disperse(f, g).apply(t)
    def measure[F[_] : Applicative, B](f: F[Unit], g: A => B) = tt.measure(f, g).apply(t)
    def measureState[S, B](f: State[S, Unit], g: A => B) = tt.measureState(f, g).apply(t)
    def assemble[B](elements: List[B])(implicit ev: A =:= Unit) = tt.assemble(elements).apply(t)
  }

}


