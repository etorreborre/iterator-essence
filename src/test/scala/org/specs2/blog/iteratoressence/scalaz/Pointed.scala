package org.specs2.blog
package iteratoressence
package scalaz
import State._

trait Pointed[F[_]] {
  def point[A](a: => A): F[A]
}

object Pointed {

  implicit def ConstIsPointed[M : Monoid] = new Pointed[({type l[A]=Const[M, A]})#l] {
    def point[A](a: => A) = Const[M, A](implicitly[Monoid[M]].z)
  }

  implicit def StateIsPointed[S] = new Pointed[({type l[A]=State[S, A]})#l] {
    def point[A](a: => A) = state[S, A](s => (s, a))
  }

  implicit def IdentIsPointed = new Pointed[Ident] {
    def point[A](a: => A): Ident[A] = Ident(a)
  }
  implicit def ListIsPointed = new Pointed[List] {
    def point[A](a: => A): List[A] = List(a)
  }

  implicit def OptionIsPointed = new Pointed[Option] {
    def point[A](a: => A): Option[A] = Option(a)
  }

  implicit def ProductIsPointed[F1[_] : Pointed, F2[_] : Pointed] = new Pointed[({type l[A]=Product[F1, F2, A]})#l] {
    val f1 = implicitly[Pointed[F1]]
    val f2 = implicitly[Pointed[F2]]

    def point[A](a: => A) = Product(f1.point(a), f2.point(a))
  }

  implicit def ComposedIsPointed[F1[_] : Pointed, F2[_] : Pointed] = new Pointed[({type l[A]=F1[F2[A]]})#l] {
    val f1 = implicitly[Pointed[F1]]
    val f2 = implicitly[Pointed[F2]]

    def point[A](a: => A) = f1.point(f2.point(a))
  }

}