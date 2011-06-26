package org.specs2.blog
package iteratoressence
package scalaz

sealed trait Tree[A] {

  import Tree._

  /**The label at the root of this tree. */
  def rootLabel: A

  /**The child nodes of this tree. */
  def subForest: Stream[Tree[A]]

}

object Tree extends Trees {
  /**Construct a tree node with no children. */
  def apply[A](root: => A): Tree[A] =
    leaf(root)
}

trait Trees {

  object Node {
    def unapply[A](t: Tree[A]): Option[(A, Stream[Tree[A]])] = Some((t.rootLabel, t.subForest))
  }

  /**Construct a new Tree node. */
  def node[A](root: => A, forest: => Stream[Tree[A]]): Tree[A] = new Tree[A] {
    lazy val rootLabel = root
    lazy val subForest = forest

    override def toString = "<tree>"
  }

  /**Construct a tree node with no children. */
  def leaf[A](root: => A): Tree[A] = node(root, Stream.empty)
}