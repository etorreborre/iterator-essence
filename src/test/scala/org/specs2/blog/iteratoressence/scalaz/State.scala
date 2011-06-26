package org.specs2.blog
package iteratoressence
package scalaz
import State._

sealed trait State[S, +A] {

  def apply(s: S): (S, A)

  def map[B](f: A => B): State[S, B] = state(apply(_) match {
    case (s, a) => (s, f(a))
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = state(apply(_) match {
    case (s, a) => f(a)(s)
  })

  def withs(f: S => S): State[S, A] = state(f andThen (apply(_)))
}

object State extends States

trait States {

  def state[S, A](f: S => (S, A)): State[S, A] = new State[S, A] {
    def apply(s: S) = f(s)
  }

  def init[S]: State[S, S] = state[S, S](s => (s, s))

  def modify[S](f: S => S) = init[S] flatMap (s => state(_ => (f(s), ())))

  implicit def KleisliComposeState[B, C, S](f: B => State[S, C]) = new KleisliComposeState[B, C, S](f)
  class KleisliComposeState[B, C, S](f: B => State[S, C]) {
    def ∎[A](g: A => State[S, B])  = (a: A) => { g(a) flatMap f }
  }

}
