package org.specs2.blog
package iteratoressence
package scalaz

import Bind._
import Functor._
import Pointed._
import Join._

trait Monad[F[_]] {

  val bind: Bind[F]
  val pointed: Pointed[F]

  def pointedFunctor: PointedFunctor[F] = new PointedFunctor[F] {
    val functor = Monad.this.functor
    val pointed = Monad.this.pointed
  }

  def applic: Applic[F] = new Applic[F] {
    def applic[A, B](f: F[A => B]) = a => bind.bind[A => B, B](ff => functor.fmap(ff)(a))(f)
  }

  def applicFunctor: ApplicFunctor[F] = new ApplicFunctor[F] {
    val applic = Monad.this.applic
    val functor = Monad.this.functor
  }

  def applicative: Applicative[F] = new Applicative[F] {
    val pointedFunctor = Monad.this.pointedFunctor
    val applic = Monad.this.applic
  }

  def functor: Functor[F] = new Functor[F] {
    def fmap[A, B](f: A => B): F[A] => F[B] = (fa: F[A]) => bd((a: A) => point(f(a))).apply(fa)
  }

  def join: Join[F] = new Join[F] {
    def join[A]: F[F[A]] => F[A] = (fa: F[F[A]]) => bd((fa: F[A]) => fa).apply(fa)
  }

  def fmap[A, B](f: A => B): F[A] => F[B]     = functor.fmap(f)
  def apply[A, B](f: F[A => B]): F[A] => F[B] = applic.applic(f)
  def bd[A, B](f: A => F[B]): F[A] => F[B]    = bind.bind(f)
  def jn[A]: F[F[A]] => F[A]                  = join.join[A]
  def point[A](a: => A): F[A]                 = pointed.point(a)

}

object Monad extends Monads

trait Monads {

  implicit def IdentIsMonad = new Monad[Ident] {
    val bind    = IdentIsBind
    val pointed = IdentIsPointed
  }

  implicit def ListIsMonad[T] = new Monad[List] {
    val bind    = ListIsBind
    val pointed = ListIsPointed
  }

  implicit def OptionIsMonad[T] = new Monad[Option] {
    val bind    = OptionIsBind
    val pointed = OptionIsPointed
  }

  implicit def StateIsMonad[S] = new Monad[({type l[A]=State[S, A]})#l] {
    val bind    = StateIsBind[S]
    val pointed = StateIsPointed[S]
  }

  implicit def KleisliCompose[B, C, M[_] : Monad](f: B => M[C]) = new KleisliCompose[B, C, M](f)
  class KleisliCompose[B, C, M[_] : Monad](f: B => M[C]) {
    val monad = implicitly[Monad[M]]
    def ∎[A](g: A => M[B]) = (a: A) => monad.bd(f).apply(g(a))
  }

}