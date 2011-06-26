package org.specs2.blog
package iteratoressence
package scalaz

trait PointedFunctor[F[_]] {
  val functor: Functor[F]
  val pointed: Pointed[F]

  def point[A](a: => A): F[A] = pointed.point(a)

  def fmap[A, B](f: A => B): F[A] => F[B] = functor.fmap(f)
}

object PointedFunctor {
  implicit def ConstIsPointedFunctor[M : Monoid] = new PointedFunctor[({type l[A]=Const[M, A]})#l] {
    val functor = Functor.ConstIsFunctor
    val pointed = Pointed.ConstIsPointed
  }

  implicit def StateIsPointedFunctor[S] = new PointedFunctor[({type l[A]=State[S, A]})#l] {
    val functor = Functor.StateIsFunctor[S]
    val pointed = Pointed.StateIsPointed[S]
  }

  implicit def ProductIsPointedFunctor[F1[_] : PointedFunctor, F2[_] : PointedFunctor] = new PointedFunctor[({type l[A]=Product[F1, F2, A]})#l] {

    implicit val f1Pointed = implicitly[PointedFunctor[F1]].pointed
    implicit val f2Pointed = implicitly[PointedFunctor[F2]].pointed

    implicit val f1Functor = implicitly[PointedFunctor[F1]].functor
    implicit val f2Functor = implicitly[PointedFunctor[F2]].functor

    val functor = Functor.ProductIsFunctor(f1Functor, f2Functor)
    val pointed = Pointed.ProductIsPointed(f1Pointed, f2Pointed)
  }

  implicit def ComposedIsPointedFunctor[F1[_] : PointedFunctor, F2[_] : PointedFunctor] = new PointedFunctor[({type l[A]=F1[F2[A]]})#l] {

    implicit val f1Pointed = implicitly[PointedFunctor[F1]].pointed
    implicit val f2Pointed = implicitly[PointedFunctor[F2]].pointed

    implicit val f1Functor = implicitly[PointedFunctor[F1]].functor
    implicit val f2Functor = implicitly[PointedFunctor[F2]].functor

    val functor = Functor.ComposedIsFunctor(f1Functor, f2Functor)
    val pointed = Pointed.ComposedIsPointed(f1Pointed, f2Pointed)
  }

}