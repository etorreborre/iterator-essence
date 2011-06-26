package org.specs2.blog
package iteratoressence

import org.specs2.mutable.Specification
import scalaz.State._

class StateSpec extends Specification {

  "Using state with a simple counter" >> {
    val count = (s: String) => state((n: Int) => (n+1, s + n))
    (count("a-") flatMap count flatMap count).apply(0) must_== (3, "a-012")
  }

  "Using state with a simple counter with init" >> {
    val count = (s: String) => state((n: Int) => (n+1, s + n))
    (count("a-") flatMap count flatMap count).apply(0) must_== (3, "a-012")
  }

  "The State Monad is not commutative" >> {
    val mx = state((n: Int) => (n+1, n+1))
    val my = state((n: Int) => (n+1, n+1))

    val xy = for {
      x <- mx
      y <- my
    } yield (x, y)

    val yx = for {
      y <- my
      x <- mx
    } yield (x, y)

    xy.apply(0) must_== (2, (1, 2))
    yx.apply(0) must_== (2, (2, 1))
  }

}