package org.specs2.blog
package iteratoressence
package scalaz

trait Applic[F[_]] {
  def applic[A, B](f: F[A => B]): F[A] => F[B]
}

object Applic {

  implicit def ConstIsApplic[M : Monoid] = new Applic[({type l[A]=Const[M, A]})#l] {
    def applic[A, B](f: Const[M, A => B]) = (c: Const[M, A]) => Const[M, B](implicitly[Monoid[M]].append(f.value, c.value))
  }

  implicit def ProductIsApplic[F1[_] : Applic, F2[_] : Applic] = new Applic[({type l[A]=Product[F1, F2, A]})#l] {
    val f1 = implicitly[Applic[F1]]
    val f2 = implicitly[Applic[F2]]

    def applic[A, B](f: Product[F1, F2, A => B]) = (c: Product[F1, F2, A]) => Product[F1, F2, B](f1.applic(f.first).apply(c.first), f2.applic(f.second).apply(c.second))
  }

}