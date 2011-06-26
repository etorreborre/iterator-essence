package org.specs2.blog
package iteratoressence
package scalaz

sealed trait BinaryTree[+A]

case class Leaf[A](a: A) extends BinaryTree[A]
case class Bin[A](left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

