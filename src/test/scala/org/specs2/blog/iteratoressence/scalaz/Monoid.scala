package org.specs2.blog
package iteratoressence
package scalaz

trait Monoid[A] {
  val zero: Zero[A]
  val semigroup: Semigroup[A]

  def z: A = zero.zero
  def append(a1: A, a2: => A): A = semigroup.append(a1, a2)
}

object Monoid extends Monoids

trait Monoids {
  def monoid[A](implicit zz: Zero[A], s: Semigroup[A]): Monoid[A] = new Monoid[A] {
    val zero = zz
    val semigroup = s
  }

  implicit def ListMonoid[A]: Monoid[List[A]] = monoid
  implicit def IntMonoid: Monoid[Int] = monoid

  implicit def MapMonoid[A, B : Monoid]: Monoid[Map[A, B]] = monoid
  implicit def MapListMonoid[A, B] = MapMonoid[A, List[B]]

}