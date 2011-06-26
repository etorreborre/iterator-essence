package org.specs2.blog
package iteratoressence
package scalaz

trait Bind[F[_]] {
  def bind[A, B](f: A => F[B]): F[A] => F[B]
}

object Bind extends Binds

trait Binds {

  implicit def IdentIsBind = new Bind[Ident] {
    def bind[A, B](f: A => Ident[B]): Ident[A] => Ident[B] = (i: Ident[A]) => f(i.value)
  }

  implicit def ListIsBind = new Bind[List] {
    def bind[A, B](f: A => List[B]): List[A] => List[B] = (i: List[A]) => i flatMap f
  }

  implicit def OptionIsBind = new Bind[Option] {
    def bind[A, B](f: A => Option[B]): Option[A] => Option[B] = (i: Option[A]) => i flatMap f
  }

  implicit def StateIsBind[S] = new Bind[({type l[A]=State[S, A]})#l] {
    def bind[A, B](f: A => State[S, B]): State[S, A] => State[S, B] = (i: State[S, A]) => i flatMap f
  }

}