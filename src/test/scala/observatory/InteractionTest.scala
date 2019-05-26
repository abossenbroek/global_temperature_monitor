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
    propChecker.check
  }

  test("Verify tile image size") {
    val temps: Map[Location, Temperature] =
      Map(Location(45.933, 126.567) -> 5.265905631659059,
        Location(28.967, 118.867) -> 17.202283105022836,
        Location(43.283, 20.8) -> 3.702731737262126,
        Location(40.45, 75.383) -> -14.972222222222221,
        Location(39.083, -76.767) -> 15.379357298474947,
        Location(42.0, 15.0) -> 16.090344337948164,
        Location(45.1, 73.967) -> 8.458291624958289,
        Location(54.183, 7.9) -> 10.556164383561642,
        Location(29.227, 47.969) -> 17.452359208523585,
        Location(49.917, -97.233) -> 18.867521367521356
      )

    val colorScale = List((-60d, Color(0, 0, 0)),
      (-50d, Color(33, 0, 107)),
      (-27d, Color(255, 0, 255)),
      (-15d, Color(0, 0, 255)),
      (0d, Color(0, 255, 255)),
      (12d, Color(255, 255, 0)),
      (32d, Color(255, 0, 0)),
      (60d, Color(255, 255, 255)))

    val zoomLevel = 19
    val testTileMaxZoom = Tile(4, 4, zoomLevel)
    val imageMaxZoom = tile(temps, colorScale, testTileMaxZoom)
    assert(imageMaxZoom.dimensions._1 === 256)
  }

  // TODO:
  // Add test where at zoom level 5, four points around the tile are low and at center even lower
  // Zoom in at level 8, to center point and ensure that mean is lower than overall at zoom level 5
  // Use colors that gradually decrease in value, from white to black

  test("Color value remains the same across zoom levels") {
    val centerTile = Location(0, 0).myTile(5)
    val centerPoint = Location(centerTile.location.lat + centerTile.latSpan / 2d,
        centerTile.location.lon + centerTile.lonSpan / 2d)
    val centerZoomIn = centerPoint.myTile(7)

    val neighboursDiff = for {
      latDiff <- Seq(.05, .95)
      lonDiff <- Seq(.05, .95)
    } yield Location(centerTile.location.lat + centerTile.latSpan * latDiff,
      centerTile.location.lon + centerTile.lonSpan * lonDiff)

    val temps: Map[Location, Temperature] = neighboursDiff.map{l => l -> 10d}.toMap ++ Map(
      centerPoint -> 0d
    )

    val colorScale = List((-5d, Color(0, 0, 0)),
       (0d, Color(0, 0, 255)),
      (5d, Color(0, 255, 0)),
      (10d, Color(255, 0, 0)))

    val imageMaxZoom = tile(temps, colorScale, centerTile)

    val imageMaxZoomMinTwo = tile(temps, colorScale, centerZoomIn)

    val dirName = s"target"
    val fullImg = s"${dirName}/fullImg.png"
    val maxLevelFn = s"${dirName}/maxLevel.png"
    val maxLevelMinTwoFn = s"${dirName}/maxMinTwoLevel.png"
    imageMaxZoom.output(new java.io.File(maxLevelFn))
    imageMaxZoomMinTwo.output(new java.io.File(maxLevelMinTwoFn))

    Visualization.visualize(temps, colorScale).output(new java.io.File(fullImg))

    assert(centerTile === tileLocation(centerTile))
  }
}
