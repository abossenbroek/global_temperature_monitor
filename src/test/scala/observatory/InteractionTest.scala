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

  test("Test subTiles coordinates works") {
    val t = Tile(0, 0, 0)
    assert(t.subTiles._1 === Tile(0, 0, 1))
    assert(t.subTiles._2 === Tile(1, 0, 1))
    assert(t.subTiles._3 === Tile(0, 1, 1))
    assert(t.subTiles._4 === Tile(1, 1, 1))

    val t2 = Tile(0, 0, 3)
    assert(t2.subTiles._1 === Tile(0, 0, 4))
    assert(t2.subTiles._2 === Tile(1, 0, 4))
    assert(t2.subTiles._3 === Tile(0, 1, 4))
    assert(t2.subTiles._4 === Tile(1, 1, 4))

    val t3 = Tile(1, 1, 1)
    assert(t3.subTiles._1 === Tile(2, 2, 2))
    assert(t3.subTiles._2 === Tile(3, 2, 2))
    assert(t3.subTiles._3 === Tile(2, 3, 2))
    assert(t3.subTiles._4 === Tile(3, 3, 2))

   val t5 = Tile(7, 7, 3)
   assert(t5.subTiles._1 === Tile(14, 14, 4))
   assert(t5.subTiles._2 === Tile(15, 14, 4))
   assert(t5.subTiles._3 === Tile(14, 15, 4))
   assert(t5.subTiles._4 === Tile(15, 15, 4))

  }

  test("Test subTiles spans entire range") {
    val zoomLevel = 3
    val res = Range(0, 1 << zoomLevel,2).map(x => {
      Range(0, (1 << zoomLevel - 1)).map(y => {
        val t = Tile(x, y, zoomLevel)
        List(t.subTiles._1.x, t.subTiles._4.x)
      }
      ).foldLeft(List[Int]())(_++_)
    }
    ).foldLeft(List[Int]())(_++_)

    assert(res === Range(0, ((1 << (zoomLevel + 1)))))

  }


//  test("Test whether pixels in image map to proper Lat Lon") {
//    val pixelTestGen = for {
//      z <- Gen.choose(1, 19)
//      tileX <- Gen.choose(0, (1 << z) - 1)
//      tileY <- Gen.choose(0, (1 << z) - 1)
//      x <- Gen.choose(0, 256)
//      y <- Gen.choose(0, 256)
//    } yield ((tileX, tileY), x, y, z)
//
//    val propChecker = Prop.forAll(pixelTestGen) { case ((tileX: Int, tileY: Int), relX: Int, relY: Int, z: Int) =>
//      val tile = Tile(tileX, tileY, z)
//      val nextTile = Tile(tileX + 1, tileY + 1, z)
//
//      val x = tile.tileSize * tile.x + relX
//      val y = tile.tileSize * tile.y + relY
//      val pixLatLon = tile.fromPixelToLocation((x, y))
//      ((pixLatLon.lat >= tile.location.lat && pixLatLon.lat < nextTile.location.lat) &&
//        (pixLatLon.lon >= tile.location.lon && pixLatLon.lon < nextTile.location.lon))
//    }
//
//    propChecker.check
//  }

//  test("Verify that tile lon span is calculated properly across zoom levels") {
//    val latTestGen = for {
//      z <- Gen.choose(1, 19)
//      xTileOne <- Gen.choose(0, (1 << z) - 1)
//      yTileOne <- Gen.choose(0, (1 << z) - 1)
//      xTileTwo <- Gen.choose(0, (1 << z) - 1).suchThat((l: Int) => l != xTileOne)
//      yTileTwo <- Gen.choose(0, (1 << z) - 1).suchThat((l: Int) => l != yTileOne)
//    } yield (z, xTileOne, yTileOne, xTileTwo, yTileTwo)
//
//    val propChecker = Prop.forAll(latTestGen) { z =>
//      val tileOne = Tile(z._2, z._3, z._1)
//      val tileTwo = Tile(z._4, z._5, z._1)
//      val diff = math.abs(tileOne.lonSpan - tileTwo.lonSpan) / tileOne.lonSpan
//      diff < 1e-4
//    }
//    propChecker.check
//  }

//  test("Verify tile image size") {
//    val temps: Map[Location, Temperature] =
//      Map(Location(45.933, 126.567) -> 5.265905631659059,
//        Location(28.967, 118.867) -> 17.202283105022836,
//        Location(43.283, 20.8) -> 3.702731737262126,
//        Location(40.45, 75.383) -> -14.972222222222221,
//        Location(39.083, -76.767) -> 15.379357298474947,
//        Location(42.0, 15.0) -> 16.090344337948164,
//        Location(45.1, 73.967) -> 8.458291624958289,
//        Location(54.183, 7.9) -> 10.556164383561642,
//        Location(29.227, 47.969) -> 17.452359208523585,
//        Location(49.917, -97.233) -> 18.867521367521356
//      )
//
//    val colorScale = List((-60d, Color(0, 0, 0)),
//      (-50d, Color(33, 0, 107)),
//      (-27d, Color(255, 0, 255)),
//      (-15d, Color(0, 0, 255)),
//      (0d, Color(0, 255, 255)),
//      (12d, Color(255, 255, 0)),
//      (32d, Color(255, 0, 0)),
//      (60d, Color(255, 255, 255)))
//
//    val zoomLevel = 19
//    val testTileMaxZoom = Tile(4, 4, zoomLevel)
//    val imageMaxZoom = tile(temps, colorScale, testTileMaxZoom)
//    assert(imageMaxZoom.dimensions._1 === 256)
//  }

  // TODO:
  // Add test where at zoom level 5, four points around the tile are low and at center even lower
  // Zoom in at level 8, to center point and ensure that mean is lower than overall at zoom level 5
  // Use colors that gradually decrease in value, from white to black

//  test("Color value remains the same across zoom levels") {
//    val centerTile = Location(0, 0).myTile(5)
//    val centerPoint = Location(centerTile.location.lat + centerTile.latSpan / 2d,
//        centerTile.location.lon + centerTile.lonSpan / 2d)
//    val centerZoomIn = centerPoint.myTile(7)
//
//    val neighboursDiff = for {
//      latDiff <- Seq(.05, .95)
//      lonDiff <- Seq(.05, .95)
//    } yield Location(centerTile.location.lat + centerTile.latSpan * latDiff,
//      centerTile.location.lon + centerTile.lonSpan * lonDiff)
//
//    val temps: Map[Location, Temperature] = neighboursDiff.map{l => l -> 10d}.toMap ++ Map(
//      centerPoint -> 0d
//    )
//
//    val colorScale = List((-5d, Color(0, 0, 0)),
//       (0d, Color(0, 0, 255)),
//      (5d, Color(0, 255, 0)),
//      (10d, Color(255, 0, 0)))
//
//    val imageMaxZoom = tile(temps, colorScale, centerTile)
//
//    val imageMaxZoomMinTwo = tile(temps, colorScale, centerZoomIn)
//
//    val dirName = s"target"
//    val fullImg = s"${dirName}/fullImg.png"
//    val maxLevelFn = s"${dirName}/maxLevel.png"
//    val maxLevelMinTwoFn = s"${dirName}/maxMinTwoLevel.png"
//    imageMaxZoom.output(new java.io.File(maxLevelFn))
//    imageMaxZoomMinTwo.output(new java.io.File(maxLevelMinTwoFn))
//
//    Visualization.visualize(temps, colorScale).output(new java.io.File(fullImg))
//
//    assert(centerTile === tileLocation(centerTile))
//  }
}
