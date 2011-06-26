package org.specs2
package blog
package iteratoressence
import scalaz._
import scalaz.Monoid._
import scalaz.Zero._
import scalaz.Semigroup._
import Traversable._
import Applicative._

/**
 * code to test the answer to https://gist.github.com/1046228
 */
class MapOfMapsSpec extends mutable.Specification {

  "I can build a map keys->monoid" >> {
    val map1 = Map(1 -> List(1, 2))
    val map2 = Map(1 -> List(3), 2 -> List(4))
    implicit val mapMonoid = MapMonoid[Int, List[Int]]

    mapMonoid.append(map1, map2) must_== Map(1 -> List(1, 2, 3), 2 -> List(4))
  }

  "I sort events per day, place and sum the goods and bads" >> {
    events.reduceMonoid(e => Map(e.day -> Map(e.placeId -> DaySummary.fromBool(e.good)))) must_==
      Map(1 -> Map(100 -> DaySummary(2, 1),
                   101 -> DaySummary(0, 2)),
          2 -> Map(100 -> DaySummary(1, 0)))
  }


  type DayNum = Int
  type PlaceId = Int
  type DayPlaceEventSummaries = Map[DayNum, Map[PlaceId, DaySummary]]

  case class Event(day: Int, hour: Int, placeId: Int, good: Boolean)
  case class DaySummary(numGood: Int = 0, numBad: Int = 0)
  object DaySummary {
    def fromBool(good: Boolean) = if (good) DaySummary(1, 0) else DaySummary(0, 1)
  }

  implicit def DaySummaryIsMonoid: Monoid[DaySummary] = monoid[DaySummary]
  implicit def DaySummaryIsZero: Zero[DaySummary] = zero(DaySummary())
  implicit def DaySummaryIsSemigroup: Semigroup[DaySummary] = semigroup(d1 => d2 => DaySummary(numGood = d1.numGood + d2.numGood, numBad = d1.numBad + d2.numBad))


  val events = List(
    Event(1, 1, 100, true),
    Event(1, 2, 100, true),
    Event(1, 3, 100, false),
    Event(1, 1, 101, false),
    Event(1, 2, 101, false),
    Event(2, 1, 100, true)
  )
}
