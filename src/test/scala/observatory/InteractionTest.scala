package observatory

import com.sksamuel.scrimage.{Pixel, PixelTools}
import org.scalatest.prop.Checkers
import org.scalatest.{FunSuite, _}
import org.scalacheck._
import org.scalacheck.Prop._

trait InteractionTest extends FunSuite with Checkers with Matchers {

  import Interaction._

  val genTilePairs: Gen[(Tile, Tile)] = for {
    zoomLevel <- Gen.choose(1,7)
    dim = (1 << zoomLevel)
    x0 <- Gen.choose(0, (dim - 2))
    y0 <- Gen.choose(0, (dim - 2))
    x1 <- Gen.choose(x0 + 1, dim - 1)
    y1 <- Gen.choose(y0 + 1, dim - 1)
  } yield (Tile(x0, y0, zoomLevel), Tile(x1, y1, zoomLevel))

  val genTile = for {
    zoomLevel <- Gen.choose(1,7)
    dim = (1 << zoomLevel)
    x <- Gen.choose(0, dim - 1)
    y <- Gen.choose(0, dim - 1)
  } yield (Tile(x, y, zoomLevel))

  test("Test for consistent latMap") {
    check{
      forAll(genTile) {
        t => t.latMap.forall(x => -85.06 < x && x < 85.06)
      }
    }

    check {
      forAll(genTilePairs) {
        case (tile0, tile1) =>
          tile0.latMap.forall(_ > tile1.latMap.max)
      }
    }
  }

  test("Test for consistent lonMap") {
    check{
      forAll(genTile) {
        t => t.latMap.forall(x => -180 < x && x < 180)
      }
    }

    check {
      forAll(genTilePairs) {
        case (tile0, tile1) =>
          tile0.lonMap.forall(_ < tile1.lonMap.min)
      }
    }
  }

  test("Whether Chicago at different zoom levels is properly located") {
    val chicagoLoc = Location(41.85, -87.65)
    val res2 = latLon2TilePixel(chicagoLoc, 2)
    assert(res2._1 === Tile(1, 1, 2))
    assert(res2._2.equals((131, 190)), s"${res2._2} does not equal (131, 190)")
  }



  //  test("Tests image size") {
  //    val temps = List((Location(45.0,-90.0),10.0), (Location(-45.0,0.0),20.0))
  //    val colScheme = List((10.0,Color(255,0,0)), (20.0,Color(0,0,255)))
  //
  //    def testDim(t: Tile): Unit = {
  //      val img = tile(temps, colScheme, t)
  //
  //      assert(img.width === 256, s"For tile $t image width should be 256 and not ${img.width}")
  //      assert(img.height === 256, s"For tile $t image height should be 256 and not ${img.width}")
  //
  //    }
  //    testDim(Tile(0, 0, 0))
  //    testDim(Tile(0, 0, 1))
  //    testDim(Tile(7, 7, 3))
  //  }

  test("Perform property testing to ensure image correctness across zoom levels") {
    val temps = Array(
      (Location(45.0, -90.0), -10.0),
      (Location(45.0, 90.0), 0.0),
      (Location(0.0, 0.0), 10.0),
      (Location(-45.0, -90.0), 30.0),
      (Location(-45.0, 90.0), 20.0))
    val colScheme = List(
      (-10.0, Color(128, 128, 128)),
      (0.0, Color(255, 0, 0)),
      (10.0, Color(0, 255, 0)),
      (20.0, Color(0, 0, 255)),
      (30.0, Color(255, 0, 255)))

    check {
      forAll(genTile) { t =>
        val img = tile(temps, colScheme, t)
        img.width === 256 & img.height === 256
      }
    }

  }

  test("Perform property testing to ensure consistency across zoom levels") {
    val temps = Array(
      (Location(45.0, -90.0), -10.0),
      (Location(45.0, 90.0), 0.0),
      (Location(0.0, 0.0), 10.0),
      (Location(-45.0, -90.0), 30.0),
      (Location(-45.0, 90.0), 20.0))
    val colScheme = List(
      (-10.0, Color(128, 128, 128)),
      (0.0, Color(255, 0, 0)),
      (10.0, Color(0, 255, 0)),
      (20.0, Color(0, 0, 255)),
      (30.0, Color(255, 0, 255)))

    val baseTile = Tile(0, 0, 0)
    val origImage = tile(temps, colScheme, baseTile)

    check {
      val zoomLevel = Gen.choose(1, 7)
      val lat = Gen.choose(-85.0511, 85.0511)
      val lon = Gen.choose(-180.0, 180.0)


      forAll(zoomLevel, lat, lon) { (dstZoom, testLat, testLon) =>
        val testLocation = Location(testLat, testLon)
        val (_, coordOrig) = latLon2TilePixel(testLocation, 0)
        val (tileDest, coordDest) = latLon2TilePixel(testLocation, dstZoom)
        val destImage = tile(temps, colScheme, tileDest)
        origImage.argb(coordOrig._1, coordOrig._2) === destImage.argb(coordDest._1, coordDest._2)
      }
    }
  }
//    consistentZoom.check
//
//  }

  //  test("Test whether new TileImage with applicable temperatures works") {
  //    val testX = 0
  //    val testY = 0
  //    val testZoom = 1
  //    val ti = TileImage(Tile(testX, testY, testZoom))
  //
  //    val tempsIn = List[(Location, Temperature)]((Location(80, -179), -20d),
  //      (Location(65, -170), -30d)
  //    )
  //
  //    val tempsOut = List[(Location, Temperature)](
  //      (Tile(testX + 1, testY + 1, testZoom).location, 20d),
  //      (Tile(testX + 1, testY, testZoom).location, 30d),
  //      (Tile(testX, testY + 1, testZoom).location, 10d),
  //      (Location(180, 85), 10d)
  //    )
  //
  //    val tiWithTemp = tileImageWithApplicableTemps(ti, tempsIn ++ tempsOut)
  //    val tempsInTile: List[(Location, Temperature)] = tiWithTemp.temperatures match {
  //      case Some(temps) => temps.toList
  //      case _ => List[(Location, Temperature)]()
  //    }
  //
  //    assert(tempsInTile.length === tempsIn.length)
  //  }

  //  test("Test whether temperature filter properly works") {
  //    val ti = TileImage(Tile(0, 0, 0)).grow(1)
  //
  //    val testTemps = List[(Location, Temperature)](
  //      (Location(80d, -179), 20d), // should end up in NW
  //      (Location(60d, 10d), 30d), // should end up in NE
  //      (Location(-60d, -170d), 10d), // should end up in SW
  //      (Location(-60d, 179d), 10d) // should end up in SE
  //    )
  //
  //
  //    def tempsInTileImage(ti: Option[TileImage]): List[(Location, Temperature)] =
  //      tempsApplicableToTile(child(ti), testTemps).toList
  //
  //    assert(tempsInTileImage(ti.NW).length === 1,
  //      f"NW has ${tempsInTileImage(ti.NW)} members of $testTemps" ++
  //        f" with child: ${child(ti.NW).location} and bottom ${child(ti.NW).bottomRight}")
  //    assert(tempsInTileImage(ti.NE).length === 1,
  //      f"NE has ${tempsInTileImage(ti.NE)} members of $testTemps" ++
  //        f" with child: ${child(ti.NE).location} and bottom ${child(ti.NE).bottomRight}")
  //    assert(tempsInTileImage(ti.SW).length === 1,
  //      f"SW has ${tempsInTileImage(ti.SW)} members for $testTemps" ++
  //        f" with child: ${child(ti.SW).location} and bottom ${child(ti.SW).bottomRight}")
  //    assert(tempsInTileImage(ti.SE).length === 1,
  //      f"SE has ${tempsInTileImage(ti.SE)} members for $testTemps" ++
  //    f" with child: ${child(ti.SE).location} and bottom ${child(ti.SE).bottomRight}")
  //  }


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
