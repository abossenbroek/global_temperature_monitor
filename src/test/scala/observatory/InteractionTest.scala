package observatory

import org.scalacheck.{Gen, Prop}
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

  test("Lat Lon conversion to Tile and back works well") {
    val loc = Location(0d, 0d)
    val centerTile = loc.myTile(2)

    assert(centerTile === Tile(2, 2, 2))
  }

  test("Lat Lon conversion to Tile and back works well at random point") {
    val loc = Location(0d, 0d)
    val centerTile = loc.myTile(5)

    assert(tileLocation(centerTile) === loc)
  }

  test("Verify that next tile is properly returned") {
    val tile = Tile(0, 0, 2)

    assert(tile.nextTile === Tile(1, 1, 2))
  }

  test("Verify that tile lat span is calculated properly") {
    val tile = Tile(0, 0, 1)
    val tileTwo = Tile(1, 0, 1)

    assert(tile.latSpan === 85.05112877980659)
    assert(tile.lonSpan === 180.0)
    assert(tileTwo.latSpan === 85.05112877980659)

  }

  test("Verify that tile lon span is calculated properly across zoom levels") {
    val latTestGen = for {
      z <- Gen.choose(1, 19)
      xTileOne <- Gen.choose(0, (1 << z) - 1)
      yTileOne <- Gen.choose(0, (1 << z) - 1)
      xTileTwo <- Gen.choose(0, (1 << z) - 1).suchThat((l: Int) => l != xTileOne)
      yTileTwo <- Gen.choose(0, (1 << z) - 1).suchThat((l: Int) => l != yTileOne)
    } yield (z, xTileOne, yTileOne, xTileTwo, yTileTwo)

    val propChecker = Prop.forAll(latTestGen) { z =>
      Tile(z._2, z._3, z._1).lonSpan == Tile(z._4, z._5, z._1).lonSpan
    }
  }
}
