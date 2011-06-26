package org.specs2.blog
package iteratoressence
package scalaz
import Monoid._

trait Semigroup[A] {
  def append(a1: A, a2: => A): A
}

object Semigroup extends Semigroups

trait Semigroups {
  def semigroup[A](k: A => (=> A) => A): Semigroup[A] = new Semigroup[A] {
    def append(a1: A, a2: => A) = k(a1)(a2)
  }

  implicit def ListSemigroup[A]: Semigroup[List[A]] = semigroup(a1 => a2 => a1 ::: a2)
  implicit def IntSemigroup: Semigroup[Int] = semigroup(a1 => a2 => a1 + a2)

  implicit def MapSemigroup[A, B : Monoid]: Semigroup[Map[A, B]] = {
    implicit val monoid = implicitly[Monoid[B]]
    semigroup { m1 => m2 =>
      m1.foldLeft(m2) { (res: Map[A, B], cur: (A, B)) =>
        val (k: A, v: B) = cur
        res + (k -> monoid.append(v, res.getOrElse(k, monoid.z)))
      }
    }
  }

}