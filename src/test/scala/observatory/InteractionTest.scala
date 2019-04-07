package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait InteractionTest extends FunSuite with Checkers {
  import Interaction._

  test("tile location is properly calculated at zoom level 1") {
    val edge11 = tileLocation(Tile(1, 1, 1))
    assert(edge11 === Location(0, 0))

    val edge01 = tileLocation(Tile(0, 1, 1))
    assert(edge01 === Location(0, -180d))

    val edge00 = tileLocation(Tile(0, 0, 1))
    assert(edge00 === Location(85.05112877980659, -180d))
  }

  test("tile location is properly calculated at zoom level 2") {
    val edge00 = tileLocation(Tile(0, 0, 2))
    assert(edge00 === Location(85.05112877980659, -180d))

    val edge22 = tileLocation(Tile(2, 2, 2))
    assert(edge22 === Location(0, 0))

    val edge01 = tileLocation(Tile(0, 1, 2))
    assert(edge01 === Location(66.51326044311186, -180d))

    // Ghost point necessary to calculate the edge
    val edge44 = tileLocation(Tile(4, 4, 2))
    assert(edge44 === Location(-85.05112877980659, 180d))

  }
}
