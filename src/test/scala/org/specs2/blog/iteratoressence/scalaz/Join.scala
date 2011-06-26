package org.specs2.blog
package iteratoressence
package scalaz

trait Join[F[_]] {
  def join[A]: F[F[A]] => F[A]
}

object Join extends Joins

trait Joins {

  implicit def IdentIsJoin = new Join[Ident] {
    def join[A]: Ident[Ident[A]] => Ident[A] = (i: Ident[Ident[A]]) => i.value
  }

  implicit def ListIsJoin = new Join[List] {
    def join[A]: List[List[A]] => List[A] = (i: List[List[A]]) => i.flatten
  }

  implicit def StateIsJoin[S] = new Join[({type l[A]=State[S, A]})#l] {
    def join[A]: State[S, State[S, A]] => State[S, A] = (i: State[S, State[S, A]]) => i flatMap identity
  }

}
