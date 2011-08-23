package org.specs2.blog
package iteratoressence

import scalaz._
import Applicative._
import Monoid._
import Monad._
import Traversable._
import State._

class BinaryTreeTraversal extends org.specs2.mutable.Specification {  

  "we can get the contents of a binary tree by traversing it" >> {
    val f = (i: Int) => List(i)
    (tree.traverse[Int, ({type l[A]=Const[List[Int], A]})#l](f)).value must_== List(1, 2)
  }
  "we can get the contents of a binary tree by reducing it with a Monoid" >> {
    (tree reduce ((i: Int) => List(i))) must_== List(1, 2)
  }
  "we can get the contents of a binary tree by using the `contents` method" >> {
    tree.contents must_== List(1, 2)
  }
  "we can get the number of elements in the binary tree by using the `count` method" >> {
    tree.count must_== 2
  }
  "we can map the number of elements in the binary tree by using the `map` method" >> {
    tree.map((i: Int) => i.toString) must_== Bin(Leaf("1"), Leaf("2"))
  }
  "we can get the shape of the tree" >> {
    tree.shape must_== Bin(Leaf(()), Leaf(()))
  }
  "we can decompose a tree between shape and contents" >> {
    tree.decompose must_== (Bin(Leaf(()), Leaf(())), List(1, 2))
  }
  "we can count elements while mapping them to something else" >> {
    val count = (i: Int) => state((n: Int) => (n+1, ()))
    val map   = (i: Int) => i.toString

    tree.collect[({type l[A]=State[Int, A]})#l, String](count, map).apply(0) must_== (2, Bin(Leaf("1"), Leaf("2")))
  }
  "we can label elements" >> {
    val labelling: State[Int, Int]      = state((n: Int) => (n+1, n+1))
    val naming: (Double, Int) => String = (p1: Double, p2: Int) => p1+" node is "+p2

    val tree: BinaryTree[Double] = Bin(Leaf(1.1), Bin(Leaf(2.2), Leaf(3.3)))

    tree.disperse[({type l[A]=State[Int, A]})#l, Int, String](labelling, naming.curried).apply(0)._2 must_==
      Bin(Leaf("1.1 node is 1"), Bin(Leaf("2.2 node is 2"), Leaf("3.3 node is 3")))
  }
  "we can 'measure' elements, where we accumulate some state independent of the element and map elements" >> {
    val crosses = state((s: String) => (s+"x", ()))
    val map     = (i: Int) => i.toString

    tree2.measureState(crosses, map).apply("") must_== ("xxx", Bin(Leaf("1"), Bin(Leaf("2"), Leaf("3"))))
  }
  "we can rebuild a tree, having the shape and the contents" >> {
    "if the number of elements is exact" >> {
      shape.assemble(List(1, 2)) must_== (List(), Some(Bin(Leaf(1), Leaf(2))))
    }
    "if there are too many elements" >> {
       shape.assemble(List(1, 2, 3)) must_== (List(3), Some(Bin(Leaf(1), Leaf(2))))
    } 
    "if there are not enough elements" >> {
       shape.assemble(List(1)) must_== (List(), None)
    }
    "naive recursive version" >> {
      def assemble(es: List[Int], s: BinaryTree[Unit]) : (List[Int], Option[BinaryTree[Int]]) = {
        (es, s) match {
          case (Nil, _)                      => (es, None)
          case (e :: rest, Leaf(()))         => (rest, Some(Leaf(e)))
          case (_, Bin(left, right))         => {
            assemble(es, left) match {
              case (l, None)       => (l, None)
              case (Nil, Some(l))  => (Nil, None)
              case (rest, Some(l)) => assemble(rest, right) match {
                case (r, None)            => (r, None)
                case (finalRest, Some(r)) => (finalRest, Some(Bin(l, r)))
              }
            }
          }
        }
      }
      assemble(List(1, 2, 3), shape) must_== (List(3), Some(Bin(Leaf(1), Leaf(2))))
      assemble(List(1), shape)       must_== (List(), None)
    }
    "for loop with a list" >> {
      def assemble[T](es: List[T], shape: List[Unit]) = {
        var elements = es
        var list: Option[List[T]] = None
        for (u <- shape) {
          if (!elements.isEmpty) {
            list match {
              case None    => list = Some(List(elements.head))
              case Some(l) => list = Some(l :+ elements.head)
            }
            elements = elements.drop(1)
          } else {
            list = None
          }
        }
        (elements, list)
      }
      assemble(List(1, 2, 3), List((), ())) must_== (List(3), Some(List(1, 2)))
      assemble(List(1), List((), ()))       must_== (List(), None)
    }
    "assemble with a List traversable" >> {
      List((), ()).assemble(List(1, 2, 3)) must_== (List(3), Some(List(1, 2)))
    }
  }
  "We can show the relationships between applicative composition and monadic composition" >> {
    "plus1" >> {
      List(1, 2, 3).traverseState(plus1).apply(1)._1 must_== 4
    }
    "times2" >> {
      List(1, 2, 3).traverseState(times2).apply(1)._1 must_== 8
    }
    "traverse times2 ∎ plus1" >> {
      List(1, 2, 3).traverseState(times2 ∎ plus1).apply(1)._1 must_== 22
    }
    "traverse times2 ∎ traverse plus1" >> {
      val times2Traverse = (l: List[Int]) => l.traverseState(times2)
      val plus1Traverse = (l: List[Int]) => l.traverseState(plus1)

      (times2Traverse ∎ plus1Traverse).apply(List(1, 2, 3)).apply(1)._1 must_== 32
    }
    "traverse plus2 ∎ traverse plus1 === traverse plus2 ∎ plus1" >> {
      val plus2Traverse = (l: List[Int]) => l.traverseState(plus2)
      val plus1Traverse = (l: List[Int]) => l.traverseState(plus1)

      (plus2Traverse ∎ plus1Traverse).apply(List(1, 2, 3)).apply(1) must_==
      List(1, 2, 3).traverseState(plus2 ∎ plus1).apply(1)
    }
    "traverse plus2 ∎ plus1 == 10" >> {
      List(1, 2, 3).traverseState(plus2 ∎ plus1).apply(1)._1 must_== 10
    }
    "traverse times2 ⊡ plus1" >> {
      val application = (i: Int) => times2(i) ⊡ plus1(i)
      List(1, 2, 3).traverseState(application).apply(1)._1 must_== 1
    }
    "traverse times2 ⊡ traverse plus1" >> {
      (List(1, 2, 3).traverseState(times2) ⊡ List(1, 2, 3).traverseState(plus1)).apply(1)._1 must_== 1
    }
    "traverse times2 ⊡ traverse plus1 == traverse times2 ⊡ plus1" >> {
      val traverseTimes2TraversePlus1 = (List(1, 2, 3).traverseState(times2) ⊡ List(1, 2, 3).traverseState(plus1)).apply(1)._2.apply(1)
      val traverseTimes2Plus1 = List(1, 2, 3).traverseState((i: Int) => times2(i) ⊡ plus1(i)).apply(1)._2.traverseState(identity _).apply(1)

      (traverseTimes2TraversePlus1 must_== traverseTimes2Plus1) and
      (traverseTimes2Plus1._1 must_== 4)
    }
  }

  val tree: BinaryTree[Int]   = Bin(Leaf(1), Leaf(2))
  val tree2: BinaryTree[Int]  = Bin(Leaf(1), Bin(Leaf(2), Leaf(3)))
  val shape: BinaryTree[Unit] = Bin(Leaf(()), Leaf(()))

  def plus1[A]  = (a: A) => state((n: Int) => (n+1, a))
  def plus2[A]  = (a: A) => state((n: Int) => (n+2, a))
  def times2[A] = (a: A) => state((n: Int) => (n*2, a))

}