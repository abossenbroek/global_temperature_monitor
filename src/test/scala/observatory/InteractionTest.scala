package observatory

import com.sksamuel.scrimage.{Pixel, PixelTools}
import org.scalatest.prop.Checkers
import org.scalatest.{FunSuite, _}

trait InteractionTest extends FunSuite with Checkers with Matchers {
  import Interaction._

  def child(childNode: Option[TileImage]): TileImage = childNode match {
    case Some(n) => n
    case _ => TileImage(Tile(0, 0, 0))
  }

  test("Test boolean operators of Location") {
    assert(Location(85, -180) > Location(-85, 180), "Location(85, -180) > Location(-85, 180) should hold")
    assert(Location(-85, 180) < Location(85, -180), "Location(-85, 180) < Location(85, -180) should hold")
  }

//  test("Test whether TileImages at zoom level 1 can rebuild zoom level 0") {
//    def createTestTileImage(x: Int, y: Int, zoom: Int, color: Pixel): TileImage = {
//      val t = Tile(x, y, zoom)
//      val img = Array.fill(t.tileSize * t.tileSize)(color)
//      TileImage(t, Some(img))
//    }
//
//    val NW = createTestTileImage(0, 0, 1, PixelTools.rgb(0, 0, 0))
//    val NE = createTestTileImage(1, 0, 1, PixelTools.rgb(64, 64, 64))
//    val SW = createTestTileImage(0, 1, 1, PixelTools.rgb(128, 128, 128))
//    val SE = createTestTileImage(1, 1, 1, PixelTools.rgb(255, 255, 255))
//
//    val topLevel = TileImage(NW, NE, SW, SE)
//    val distinctPixels = topLevel.pixelArray match {
//      case Some(pl) => pl.distinct.length
//      case _ => 0}
//
//    assert(topLevel.x === 0)
//    assert(topLevel.y === 0)
//    assert(topLevel.zoom === 0)
//    assert(distinctPixels === 4)
//  }

  test("Whether center of tile is properly calculated") {
    val t = Tile(0, 0, 0)
    val center = centerOfTile(t)

    assert(center ~= Location(0, 0))
  }

//  test("Test whether TileImages depth level 0 and 1 works") {
//    def createTestTileImage(x: Int, y: Int, zoom: Int, color: Pixel): TileImage = {
//      val t = Tile(x, y, zoom)
//      val img = Array.fill(t.tileSize * t.tileSize)(color)
//      TileImage(t, Some(img))
//    }
//
//    val NW = createTestTileImage(0, 0, 1, PixelTools.rgb(0, 0, 0))
//    val NE = createTestTileImage(1, 0, 1, PixelTools.rgb(64, 64, 64))
//    val SW = createTestTileImage(0, 1, 1, PixelTools.rgb(128, 128, 128))
//    val SE = createTestTileImage(1, 1, 1, PixelTools.rgb(255, 255, 255))
//
//    val topLevel = TileImage(NW, NE, SW, SE)
//
//    assert(topLevel.depth === 1)
//    assert(NE.depth === 0)
//  }

  test("Test whether empty initial node can be grown to depth 1") {
    val topLevel = TileImage(Tile(0, 0, 0))

    val depthOne = topLevel.grow(1)

    assert(depthOne.depth === 1)

    assert(child(depthOne.NW).t === Tile(0, 0, 1))
    assert(child(depthOne.NE).t === Tile(1, 0, 1))
    assert(child(depthOne.SW).t === Tile(0, 1, 1))
    assert(child(depthOne.SE).t === Tile(1, 1, 1))
  }

  test("Test whether empty initial node can be grown to depth 1 with bottom corners") {
    val depthOne = TileImage(Tile(0, 0, 0)).grow(1)

    assert(depthOne.depth === 1)

    def testBottom(name: String, c: Option[TileImage], x: Int, y: Int): Unit = {
      assert(child(c).bottomRight ~= Tile(x, y, 1).location,
        f"$name ${child(c).bottomRight} is not ${Tile(x, y,1).location} ")
    }

    def testBottomLocation(name: String, c: Option[TileImage], lat: Double, lon: Double): Unit = {
      assert(child(c).bottomRight ~= Location(lat, lon),
        f"$name ${child(c).bottomRight} is not ${Location(lat, lon)} ")
    }

    testBottom("NW", depthOne.NW, 1, 1)
    testBottom("NE", depthOne.NE, 2, 1)
    testBottom("SW", depthOne.SW, 1, 2)
    testBottom("SE", depthOne.SE, 2, 2)

    testBottomLocation("NW", depthOne.NW, 0, 0)
    testBottomLocation("NE", depthOne.NE, 0, 180)
    testBottomLocation("SW", depthOne.SW, -85.05113, 0)
    testBottomLocation("SE", depthOne.SE, -85.05113, 180)
  }


  test("Test whether empty initial node can be grown to depth 2") {
    val topLevel = TileImage(Tile(0, 0, 0))

    val depthTwo = topLevel.grow(2)

    assert(depthTwo.depth === 2)

    def child(childNode: Option[TileImage]): TileImage = childNode match {
      case Some(n) => n
      case _ => TileImage(Tile(0, 0, 0))
    }

    assert(child(depthTwo.NW).depth === 1)
    assert(child(child(depthTwo.NW).NW).t === Tile(0, 0, 2))
    assert(child(child(depthTwo.NW).NE).t === Tile(1, 0, 2))
    assert(child(child(depthTwo.NW).SW).t === Tile(0, 1, 2))
    assert(child(child(depthTwo.NW).SE).t === Tile(1, 1, 2))

    assert(child(child(depthTwo.NE).NW).t === Tile(2, 0, 2))
    assert(child(child(depthTwo.NE).NE).t === Tile(3, 0, 2))
    assert(child(child(depthTwo.NE).SW).t === Tile(2, 1, 2))
    assert(child(child(depthTwo.NE).SE).t === Tile(3, 1, 2))

    assert(child(child(depthTwo.SW).NW).t === Tile(0, 2, 2))
    assert(child(child(depthTwo.SW).NE).t === Tile(1, 2, 2))
    assert(child(child(depthTwo.SW).SW).t === Tile(0, 3, 2))
    assert(child(child(depthTwo.SW).SE).t === Tile(1, 3, 2))

    assert(child(child(depthTwo.SE).NW).t === Tile(2, 2, 2))
    assert(child(child(depthTwo.SE).NE).t === Tile(3, 2, 2))
    assert(child(child(depthTwo.SE).SW).t === Tile(2, 3, 2))
    assert(child(child(depthTwo.SE).SE).t === Tile(3, 3, 2))
  }

  test("Test whether lat and lon range are correct") {
    val rootNode = TileImage(Tile(0, 0, 0)).grow(2)
    assert((child(child(rootNode.NW).NW).tileLonRange
      + child(child(rootNode.NW).NE).tileLonRange
      + child(child(rootNode.NE).NW).tileLonRange
      + child(child(rootNode.NE).NE).tileLonRange) === rootNode.tileLonRange,
      "Sum of lonRange of four children should equal tile 0 lonRange")

    assert((child(child(rootNode.NW).NW).tileLatRange
      + child(child(rootNode.NW).SW).tileLatRange
      + child(child(rootNode.SW).NW).tileLatRange
      + child(child(rootNode.SW).SW).tileLatRange) === rootNode.tileLatRange,
      "Sum of latRange of four children should equal tile 0 latRange")
  }

  test("Test that lat/lon coordinates are well mapped") {
    val rootNode = TileImage(Tile(0, 0, 0)).grow(2)
    val nwNw = child(child(rootNode.NW).NW)
    val nwSe = child(child(rootNode.NW).SE)
    val topLocation = nwNw.tileCoordinate(0)
    val bottomLocation = nwNw.tileCoordinate((tileWidth * tileWidth - 1).toInt)
    assert(topLocation === nwNw.t.location, "Test accuracy of (0) tile coordinate")
    assert(Location(bottomLocation.lat - nwNw.latIdx, bottomLocation.lon + nwNw.lonIdx)  === nwSe.t.location, f"Test accuracy of (${(tileWidth * tileWidth - 1).toInt}) tile coordinate")
  }

  test("Test getTileImage") {
    val rootNode = TileImage(Tile(0, 0, 0)).grow(3)

    def testNode(t: Tile) =
      assert(rootNode.getTileImage(t).getOrElse(TileImage(Tile(0, 0, 0))).t === t,
        f"Tile $t should be in tree")

    testNode(Tile(1, 1, 2))
    testNode(Tile(1, 1, 3))
    testNode(Tile(0, 0, 0))

    assert(rootNode.getTileImage(Tile(1, 1, 4)).isEmpty, "We shouldn't find tile that is non-existent")
  }

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

  test("Test whether temperature filter properly works") {
    val ti = TileImage(Tile(0, 0, 0)).grow(1)

    val testTemps = List[(Location, Temperature)](
      (Location(80d, -179), 20d), // should end up in NW
      (Location(60d, 10d), 30d), // should end up in NE
      (Location(-60d, -170d), 10d), // should end up in SW
      (Location(-60d, 179d), 10d) // should end up in SE
    )


    def tempsInTileImage(ti: Option[TileImage]): List[(Location, Temperature)] =
      tempsApplicableToTile(child(ti), testTemps).toList

    assert(tempsInTileImage(ti.NW).length === 1,
      f"NW has ${tempsInTileImage(ti.NW)} members of $testTemps" ++
        f" with child: ${child(ti.NW).location} and bottom ${child(ti.NW).bottomRight}")
    assert(tempsInTileImage(ti.NE).length === 1,
      f"NE has ${tempsInTileImage(ti.NE)} members of $testTemps" ++
        f" with child: ${child(ti.NE).location} and bottom ${child(ti.NE).bottomRight}")
    assert(tempsInTileImage(ti.SW).length === 1,
      f"SW has ${tempsInTileImage(ti.SW)} members for $testTemps" ++
        f" with child: ${child(ti.SW).location} and bottom ${child(ti.SW).bottomRight}")
    assert(tempsInTileImage(ti.SE).length === 1,
      f"SE has ${tempsInTileImage(ti.SE)} members for $testTemps" ++
    f" with child: ${child(ti.SE).location} and bottom ${child(ti.SE).bottomRight}")
  }

  test("Test whether insert into zoom 1 tree works with edge cases") {
    val ti = TileImage(Tile(0, 0, 0)).grow(1)

    val testTemps = List[(Location, Temperature)](
      (Tile(0,0, 1).location, 20d), // should end up in NW
      (Tile(1, 0, 1).location, 30d), // should end up in NE
      (Tile(0, 1, 1).location, 10d), // should end up in SE
      (Tile(1, 1, 1).location, 10d) // should end up in SW
    )

    def tempsInTileImage(ti: Option[TileImage]): List[(Location, Temperature)] =
      tempsApplicableToTile(child(ti), testTemps).toList


    def testLengthTemps(name: String, c: Option[TileImage]): Unit = {
      assert(tempsInTileImage(c).length === 1,
        f"$name has ${tempsInTileImage(c)} members of $testTemps")
    }

    testLengthTemps("NW", ti.NW)
    testLengthTemps("NE", ti.NE)
    testLengthTemps("SW", ti.SW)
    testLengthTemps("SE", ti.SE)
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
