package org.specs2.blog
package iteratoressence
package scalaz

trait ApplicFunctor[F[_]] {
  val applic: Applic[F]
  val functor: Functor[F]

  def fmap[A, B](f: A => B): F[A] => F[B] = functor.fmap(f)

  def apply[A, B](f: F[A => B]): F[A] => F[B] = applic.applic(f)

  def liftA2[A, B, C](f: A => B => C): F[A] => F[B] => F[C] = a => applic.applic(functor.fmap(f)(a))

}

object ApplicFunctor {
  implicit def ConstIsApplicFunctor[M : Monoid] = new ApplicFunctor[({type l[A]=Const[M, A]})#l] {
    val applic = Applic.ConstIsApplic
    val functor = Functor.ConstIsFunctor
  }

  implicit def ProductIsApplicFunctor[F1[_] : ApplicFunctor, F2[_] : ApplicFunctor] = new ApplicFunctor[({type l[A]=Product[F1, F2, A]})#l] {
    implicit val f1Applic = implicitly[ApplicFunctor[F1]].applic
    implicit val f2Applic = implicitly[ApplicFunctor[F2]].applic

    implicit val f1Functor = implicitly[ApplicFunctor[F1]].functor
    implicit val f2Functor = implicitly[ApplicFunctor[F2]].functor

    val applic = Applic.ProductIsApplic(f1Applic, f2Applic)
    val functor = Functor.ProductIsFunctor(f1Functor, f2Functor)
  }

  implicit def ComposedIsApplicFunctor[F1[_] : ApplicFunctor, F2[_] : ApplicFunctor] = new ApplicFunctor[({type l[A]=F1[F2[A]]})#l] {
    implicit val f1ApplicFunctor = implicitly[ApplicFunctor[F1]]
    implicit val f2ApplicFunctor = implicitly[ApplicFunctor[F2]]

    val functor = Functor.ComposedIsFunctor(f1ApplicFunctor.functor, f2ApplicFunctor.functor)

    val applic = new Applic[({type l[A]=F1[F2[A]]})#l] {
      def applic[A, B](f: F1[F2[A => B]]) = (c: F1[F2[A]]) => {
        f1ApplicFunctor.liftA2((ff: F2[A => B]) => f2ApplicFunctor.apply(ff))(f).apply(c)
      }
    }
  }

}