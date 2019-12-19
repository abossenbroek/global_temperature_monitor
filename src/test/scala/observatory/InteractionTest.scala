package observatory

import com.sksamuel.scrimage.{Pixel, PixelTools}
import org.scalatest.prop.Checkers
import org.scalatest.{FunSuite, _}
import org.scalacheck._
import org.scalacheck.Prop._

trait InteractionTest extends FunSuite with Checkers with Matchers {

  import Interaction._

  test("Test boolean operators of Location") {
    assert(Location(85, -180) > Location(-85, 180), "Location(85, -180) > Location(-85, 180) should hold")
    assert(Location(-85, 180) < Location(85, -180), "Location(-85, 180) < Location(85, -180) should hold")
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


  test("Test latLonPixel function") {
    def testLatLonPixel(l: Location, t: Tile, c: (Int, Int)): Unit = {
      val res = latLonPixel(l, t.zoom)
      assert(res._1 === t, s"At ${t.zoom}: $l should be in tile $t not ${res._1}")
      assert(res._2.equals(c), s"At ${t.zoom}: $l should by at coordinate $c not ${res._2}")
    }

    testLatLonPixel(Location(-85.0511, 180), Tile(0, 0, 0), (255, 255))
    testLatLonPixel(Location(-85.0511, 180), Tile(3, 3, 2), (255, 255))
    testLatLonPixel(Location(85.0511, -180), Tile(0, 0, 2), (0, 0))
    testLatLonPixel(Location(0, 0), Tile(1, 1, 2), (255, 255))
    testLatLonPixel(Location(0, 0), Tile(0, 0, 1), (255, 255))
  }

  //  test("Tests consistency of generated images 1") {
  //    val temps = List((Location(45.0,-90.0),10.0), (Location(-45.0,0.0),20.0))
  //    val colScheme = List((10.0,Color(255,0,0)), (20.0,Color(0,0,255)))
  //
  //    val imgTile000 = tile(temps, colScheme, Tile(0, 0, 0))
  //    val imgTile001 = tile(temps, colScheme, Tile(0, 0, 1))
  //    val imgTile773 = tile(temps, colScheme, Tile(7, 7, 3))
  //
  //    imgTile000.output(new java.io.File("target/imgTile1_000.png"))
  //    imgTile001.output(new java.io.File("target/imgTile1_001.png"))
  //    imgTile773.output(new java.io.File("target/imgTile1_773.png"))
  //  }
  //
  test("Tests consistency of generated images 2") {
    val temps = Array((Location(45.0, -90.0), 20.0), (Location(45.0, 90.0), 0.0), (Location(0.0, 0.0), 10.0),
      (Location(-45.0, -90.0), 0.0), (Location(-45.0, 90.0), 20.0))
    val colScheme = List((0.0, Color(255, 0, 0)), (10.0, Color(0, 255, 0)), (20.0, Color(0, 0, 255)))
    val testLocation = Location(-27.059125784374057, -180.0)
    val coordsZoomLevel0 = latLonPixel(testLocation, 0)

    val imgTileZoomLevel0 = tile(temps, colScheme, Tile(0, 0, 0))
    val testColor0 = imgTileZoomLevel0.argb(coordsZoomLevel0._2._1, coordsZoomLevel0._2._2)
    val coordsZoomLevel1 = latLonPixel(testLocation, 1)
    val imgTileZoomLevel1 = tile(temps, colScheme, coordsZoomLevel1._1)

    val testColor1 = imgTileZoomLevel1.argb(coordsZoomLevel1._2._1, coordsZoomLevel1._2._2)

    assert(testColor0 === testColor1,
      s"Colors should be same at level 0 ($coordsZoomLevel0) and 1 ($coordsZoomLevel1)")

    val coordsZoomLevel2 = latLonPixel(testLocation, 2)
    val imgTileZoomLevel2 = tile(temps, colScheme, coordsZoomLevel2._1)
    assert(coordsZoomLevel2._2._1 <= imgTileZoomLevel2.width,
      s"${coordsZoomLevel2._2._1} should fit in ${imgTileZoomLevel2.width}")
    assert(coordsZoomLevel2._2._2 <= imgTileZoomLevel2.height,
      s"${coordsZoomLevel2._2._2} should fit in ${imgTileZoomLevel2.height}")

    val testColor2 = imgTileZoomLevel2.argb(coordsZoomLevel2._2._1, coordsZoomLevel2._2._2)

    assert(testColor0 === testColor2,
      s"At level 0 ($coordsZoomLevel0) and 2 ($coordsZoomLevel2), found respectively $testColor0 and $testColor2")
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

    val property = {
      val zoomLevel = Gen.choose(1, 7)
      val lat = Gen.choose(-85.0511, 85.0511)
      val lon = Gen.choose(-180.0, 180.0)


      forAll(zoomLevel, lat, lon) { (dstZoom, testLat, testLon) =>
        val testLocation = Location(testLat, testLon)
        val (_, coordOrig) = latLonPixel(testLocation, 0)
        val (tileDest, coordDest) = latLonPixel(testLocation, dstZoom)
        val destImage = tile(temps, colScheme, tileDest)
        origImage.argb(coordOrig._1, coordOrig._2) === destImage.argb(coordDest._1, coordDest._2)
      }
    }

    check(property)
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
