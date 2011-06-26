package org.specs2.blog
package iteratoressence
package scalaz

import Monad._

trait Applicative[F[_]] {
  type FT[_] = F[_]

  val pointedFunctor: PointedFunctor[F]
  val applic: Applic[F]

  def functor: Functor[F] = new Functor[F] {
    def fmap[A, B](f: A => B) = pointedFunctor fmap f
  }

  def pointed: Pointed[F] = new Pointed[F] {
    def point[A](a: => A) = pointedFunctor point a
  }

  def applicFunctor: ApplicFunctor[F] = new ApplicFunctor[F] {
    val applic = Applicative.this.applic
    val functor = pointedFunctor.functor
  }

  def fmap[A, B](f: A => B): F[A] => F[B]     = functor.fmap(f)
  def point[A](a: => A): F[A]                 = pointed.point(a)
  def apply[A, B](f: F[A => B]): F[A] => F[B] = applic.applic(f)
}

object Applicative extends Applicatives {

  implicit def ApplySyntax[A, B, F[_] : Applicative](f: F[A => B]): ApplySyntax[A, B, F] = new ApplySyntax[A, B, F](f)

  class ApplySyntax[A, B, F[_] : Applicative](f: F[A => B]) {
    def <*>(fa: F[A]) = implicitly[Applicative[F]].apply(f).apply(fa)
  }

  implicit def ApplySyntax2[A, B](f: A => B): ApplySyntax2[A, B]= new ApplySyntax2(f)

  class ApplySyntax2[A, B](f: A => B) {
    def ∘[F[_] : Applicative](fa: F[A]) = implicitly[Applicative[F]].point(f) <*> fa
  }

}

trait Applicatives {

  implicit def liftConst[A, B, M : Monoid](f: A => M): A => Const[M, B] = (a: A) => Const[M, B](f(a))

  implicit def StateIsApplicative[T] = MonadIsApplicative[({type l[A]=State[T, A]})#l](StateIsMonad[T])

  implicit def ConstIsApplicative[M : Monoid] = new Applicative[({type l[A]=Const[M, A]})#l] {
    val pointedFunctor = PointedFunctor.ConstIsPointedFunctor
    val applic = Applic.ConstIsApplic
  }

  implicit def MonadIsApplicative[T[_] : Monad] = new Applicative[T] {
    val pointedFunctor = implicitly[Monad[T]].pointedFunctor
    val applic = implicitly[Monad[T]].applic
  }

  implicit def ProductIsApplicative[F1[_] : Applicative, F2[_] : Applicative] = new Applicative[({type l[T] = Product[F1, F2, T]})#l] {
    implicit val f1PointedFunctor = implicitly[Applicative[F1]].pointedFunctor
    implicit val f2PointedFunctor = implicitly[Applicative[F2]].pointedFunctor

    implicit val f1Applic = implicitly[Applicative[F1]].applic
    implicit val f2Applic = implicitly[Applicative[F2]].applic


    val pointedFunctor = PointedFunctor.ProductIsPointedFunctor(f1PointedFunctor, f2PointedFunctor)
    val applic = Applic.ProductIsApplic(f1Applic, f2Applic)
  }

  implicit def ProductWithListIsApplicative[A[_] : Applicative, B] = ProductIsApplicative[A, ({type l1[U] = Const[List[B], U]})#l1]
    
  implicit def ComposedIsApplicative[F1[_] : Applicative, F2[_] : Applicative] = new Applicative[({type l[T] = F1[F2[T]]})#l] {
    implicit val f1PointedFunctor = implicitly[Applicative[F1]].pointedFunctor
    implicit val f2PointedFunctor = implicitly[Applicative[F2]].pointedFunctor

    implicit val f1ApplicFunctor = implicitly[Applicative[F1]].applicFunctor
    implicit val f2ApplicFunctor = implicitly[Applicative[F2]].applicFunctor


    val pointedFunctor = PointedFunctor.ComposedIsPointedFunctor(f1PointedFunctor, f2PointedFunctor)
    val applic         = ApplicFunctor.ComposedIsApplicFunctor(f1ApplicFunctor, f2ApplicFunctor).applic
  }

  implicit def ApplicativeProduct[A, F1[_]](app1: F1[A]) = new ApplicativeProduct[A, F1](app1)
  class ApplicativeProduct[A, F1[_]](app1: F1[A]) {
    def ⊗[F2[_]](app2: F2[A]) = Product[F1, F2, A](app1, app2)
  }

  implicit def ApplicativeComposed[F1[_] : Applicative](app1: F1[_]) = new ApplicativeComposed[F1](app1)
  class ApplicativeComposed[F1[_] : Applicative](app1: F1[_]) {
    val f1 = implicitly[Applicative[F1]]
    def ⊡[A, F2[A]](app2: F2[A]) = f1.point(app2)
  }

  implicit def ApplicativeStateComposed[S, A](s1: State[S, A]) = new ApplicativeStateComposed[S, A](s1)
  class ApplicativeStateComposed[S, A](s1: State[S, A]) {
    val f1 = StateIsApplicative[S]
    def ⊡(s2: State[S, A]): State[S, State[S, A]] = f1.point(s2)
  }

}