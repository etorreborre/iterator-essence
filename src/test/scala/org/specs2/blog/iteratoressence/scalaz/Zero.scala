package org.specs2.blog
package iteratoressence
package scalaz

trait Zero[A] {
  val zero: A
}

object Zero extends Zeros

trait Zeros {
  def zero[A](a: A): Zero[A] = new Zero[A] {
    val zero = a
  }

  implicit def ListZero[A]: Zero[List[A]] = zero(Nil)
  implicit def IntZero: Zero[Int] = zero(0)
  implicit def MapZero[A, B : Monoid]: Zero[Map[A, B]] = zero[Map[A, B]](Map())

}