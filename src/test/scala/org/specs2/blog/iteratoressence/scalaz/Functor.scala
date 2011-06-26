package org.specs2.blog
package iteratoressence
package scalaz

trait Functor[F[_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]
}

object Functor {
  implicit def ConstIsFunctor[M : Monoid] = new Functor[({type l[A]=Const[M, A]})#l] {
    def fmap[A, B](f: A => B) = (c: Const[M, A]) => Const[M, B](c.value)
  }

  implicit def IdentIsFunctor = new Functor[Ident] {
    def fmap[A, B](f: A => B): Ident[A] => Ident[B] = (i: Ident[A]) => Ident(f(i.value))
  }

  implicit def ListIsFunctor = new Functor[List] {
    def fmap[A, B](f: A => B): List[A] => List[B] = (i: List[A]) => i map f
  }

  implicit def StateIsFunctor[S] = new Functor[({type l[A]=State[S, A]})#l] {
    def fmap[A, B](f: A => B) = (c: State[S, A]) => c map f
  }

  implicit def ProductIsFunctor[F1[_] : Functor, F2[_] : Functor] = new Functor[({type l[A]=Product[F1, F2, A]})#l] {
    val f1 = implicitly[Functor[F1]]
    val f2 = implicitly[Functor[F2]]

    def fmap[A, B](f: A => B) = (c: Product[F1, F2, A]) => Product[F1, F2, B](f1.fmap(f).apply(c.first), f2.fmap(f).apply(c.second))
  }

  implicit def ComposedIsFunctor[F1[_] : Functor, F2[_] : Functor] = new Functor[({type l[A]=F1[F2[A]]})#l] {
    val f1 = implicitly[Functor[F1]]
    val f2 = implicitly[Functor[F2]]

    def fmap[A, B](f: A => B) = (c: F1[F2[A]]) => f1.fmap[F2[A], F2[B]](f2value => f2.fmap(f).apply(f2value)).apply(c)
  }

}