package observatory

import java.io.File

import com.sksamuel.scrimage.ScaleMethod.FastScale
import com.sksamuel.scrimage.{Image, Pixel, PixelTools}

import scala.collection.immutable.TreeMap

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {
  import Visualization._
  val tileWidth = 128f
  val alpha = 70

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    tile.location
  }

  def combineTiles(NW: Array[Pixel], NE: Array[Pixel], SW: Array[Pixel], SE: Array[Pixel]) : Array[Pixel] = {
    def combineCubes(a: Array[Pixel], b: Array[Pixel]): Array[Pixel] = {
      val height = math.sqrt(a.length).toInt
      Range(0, height).map{i => {
        val start = i * height
        val end = (i + 1) * height
        a.slice(start, end) ++ b.slice(start, end)
      }
      }.foldLeft(List[Pixel]())(_++_).toArray
    }

    val top = combineCubes(NW, NE)
    val bottom = combineCubes(SW, SE)
    top ++ bottom
  }

  case class InvalidTiles() extends Exception()
  case class InvalidTree() extends Exception()

  def tileImageWithChildren(t: Tile, NW: TileImage, NE: TileImage, SW: TileImage, SE: TileImage): TileImage =
    new TileImage(t, None, Some(NW), Some(NE), Some(SW), Some(SE))

  def tempsApplicableToTile(ti: TileImage, temperatures: Iterable[(Location, Temperature)]): Iterable[(Location, Temperature)] =
    temperatures.filter{temp => (ti.location >~= temp._1) && temp._1 > ti.bottomRight}

  case class TileImage(t: Tile,
                       image: Option[Array[Pixel]],
                       NW: Option[TileImage],
                       NE: Option[TileImage],
                       SW: Option[TileImage],
                       SE: Option[TileImage]) {
    lazy val zoom: Int = t.zoom
    lazy val x: Int = t.x
    lazy val y: Int = t.y
    lazy val location: Location = t.location
    lazy val bottomRight: Location = Tile(x + 1, y + 1, zoom).location
    lazy val hasChildren: Boolean = NW.nonEmpty && NE.nonEmpty && SW.nonEmpty && SE.nonEmpty
    lazy val isRoot: Boolean = (zoom == 0) && hasChildren
    lazy val tileLatRange = location.lat - bottomRight.lat
    lazy val tileLonRange = -(location.lon - bottomRight.lon)
    lazy val latIdx = tileLatRange / tileWidth
    lazy val lonIdx = tileLonRange / tileWidth


    def depth(): Int = (NW, NE, SW, SE) match {
      case (Some(nw), Some(ne), Some(sw), Some(se)) => 1 + List[Int](nw.depth, ne.depth, sw.depth, se.depth).min
      case _ => 0
    }

    private def indices(i: Int): (Int, Int) = {
      val rowNum = math.floor(i / tileWidth)
      val colNum = i - tileWidth * rowNum
      (rowNum.toInt, colNum.toInt)
    }

    private lazy val latMap = (0 until tileWidth.toInt).map { i =>
      t.location.lat - latIdx * i
    }

    private lazy val lonMap = (0 until tileWidth.toInt).map { i =>
      t.location.lon + lonIdx * i
    }

    def tileCoordinate(i: Int) : Location = {
      val (lat, lon) = indices(i)
      Location(latMap(lat), lonMap(lon))
    }

    def visualize(temps: => Iterable[(Location, Temperature)]) : TileImage = {
      val tempScale = calculateScale(colorScale)
      visualize(temps, tempScale)
    }

    def visualize(temps: => Iterable[(Location, Temperature)], tempScale: => TreeMap[Temperature, Color]): TileImage = (NW, NE, SW, SE) match {
      case (Some(nw), Some(ne), Some(sw), Some(se)) =>
        val newNW = nw.visualize(temps, tempScale)
        val newNE = ne.visualize(temps, tempScale)
        val newSW = sw.visualize(temps, tempScale)
        val newSE = se.visualize(temps, tempScale)

        def getImage(c: TileImage): Array[Pixel] = c.image.getOrElse(Array[Pixel]())

        val newImage = combineTiles(getImage(newNW), getImage(newNE), getImage(newSW), getImage(newSE)).toArray
        new TileImage(t, Some(newImage), Some(newNW), Some(newNE), Some(newSW), Some(newSE))
      case _ =>
        val tileCoords = 0 until (tileWidth * tileWidth).toInt
        val tileColors = tileCoords.map { i => {
          val currentLocation = tileCoordinate(i)
          val currentTemp = predictTemperature(temps, currentLocation)
          val col = interpolateColorWithScale(tempScale, currentTemp)
          Pixel(PixelTools.argb(alpha, col.red, col.green, col.blue))
        }
        }
        val imgArray = tileColors.toArray
        val img = Image(tileWidth.toInt, tileWidth.toInt, imgArray)
        val scaledImg = img.scaleTo(256, 256, FastScale)
        // TODO: consider saving image for easy saving
        this.copy(image = Some(scaledImg.pixels))
    }

    def getTileImage(target: Tile): Option[TileImage] = {
      if (target == t) return Some(this)
      (NW, NE, SW, SE) match {
        case (Some(nw), Some(ne), Some(sw), Some(se)) =>
          val tiles = List(nw, ne, sw, se)
          tiles.map{ti =>
            val res = ti.getTileImage(target)
            if (res.nonEmpty) return res
          }
          None
        case (_, _, _, _) =>
          None
      }
    }

    def grow(levels: Int): TileImage = (levels, NW, NE, SW, SE) match {
      case (0, _, _, _, _) =>  this
      case (i, None, None, None, None) =>
        val newLevels = i - 1
        val NW = TileImage(Tile(t.x * 2, t.y * 2, t.zoom + 1), None).grow(newLevels)
        val NE = TileImage(Tile(t.x * 2 + 1, t.y * 2, t.zoom + 1), None).grow(newLevels)
        val SW = TileImage(Tile(t.x * 2, t.y * 2 + 1, t.zoom + 1), None).grow(newLevels)
        val SE = TileImage(Tile(t.x * 2 + 1, t.y * 2 + 1, t.zoom + 1), None).grow(newLevels)
        tileImageWithChildren(t, NW, NE, SW, SE)
      case (i, Some(nw), Some(ne), Some(sw), Some(se)) =>
        val newLevels = i -1
        val NW = nw.grow(newLevels)
        val NE = ne.grow(newLevels)
        val SW = sw.grow(newLevels)
        val SE = se.grow(newLevels)
        tileImageWithChildren(t, NW, NE, SW, SE)
      case _ => throw new InvalidTree
    }
  }

  object TileImage {
    def apply(NW: TileImage, NE: TileImage, SW: TileImage, SE: TileImage): TileImage = {
      if (!(((NW.x + 1) == NE.x) && ((NW.y + 1) == SW.y) && (NW.y == NE.y) && (SW.y == SE.y) && (SW.x + 1) == SE.x)) {
        throw InvalidTiles()
      }
      val t = Tile(NW.x / 2, NW.y / 2, NW.zoom - 1)
      val image = (NW.image, NE.image, SW.image, SE.image) match {
        case (Some(nw), Some(ne), Some(sw), Some(se)) =>
          Some(combineTiles(nw, ne, sw, se))
        case _ =>
          None
      }
      new TileImage(t, image, Some(NW), Some(NE), Some(SW), Some(SE))
    }

    def apply(t: Tile, image: Option[Array[Pixel]]): TileImage = {
      new TileImage(t, image, None, None, None, None)
    }

    def apply(t: Tile): TileImage = {
      new TileImage(t, None, None, None, None, None)
    }
  }

  def centerOfTile(t: Tile): Location = {
    val latSlope = (GlobalCoordinates.TopLeft.lat - GlobalCoordinates.BottomRight.lat) / t.numTiles
    val lonSlope = -(GlobalCoordinates.TopLeft.lon - GlobalCoordinates.BottomRight.lon) / t.numTiles
    val centerLat = GlobalCoordinates.TopLeft.lat - (t.y + 0.5) * latSlope
    val centerLon = GlobalCoordinates.TopLeft.lon + (t.x + 0.5) * lonSlope
    Location(centerLat, centerLon)
  }
  
  

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
//  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
//     // Tile can be split up in recursion, results can be merged back together
//
//
//
//    // TODO:
//    // 1. Determine left and right most x
//    // 2. Determine top and bottom most y
//
//
//    // TODO: add check to map indices back to proper lat lon with slippy map tilenames
//    // Check possible use of https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Subtiles
////    def indices(i: Int): (Int, Int) = {
////      val rowNum = math.floor(i / width)
////      val colNum = i - width * rowNum
////      (rowNum.toInt, colNum.toInt)
////    }
////
////    // TODO: these are pure x, y pixels values and not mercator mapping
////    val latMap = (0 until width.toInt).map { i =>
////      tile.location.lat + latIdx * i
////    }
////
////    val lonMap = (0 until width.toInt).map { i =>
////      tile.location.lon + lonIdx * i
////    }
////
//    //    val tempScale = Visualization.calculateScale(colors)
//
//    val palette = List[(Int, Int, Int)](
//      (0,0,0),
//      (87, 87, 87),
//      (173,35,35),
//      (42, 75, 215),
//      (29, 105, 20),
//      (129,74,25),
//      (129,38,192),
//      (160,160,160),
//      (129,197,122),
//      (157,175,255),
//      (41,208,208),
//      (255,146,51),
//      (255,238,51),
//      (233,222,187),
//      (255,205,243),
//      (255,255,255)
//    )
//
//    // TODO: write recursive function that builds tile from smaller tiles
//    // TODO: write smallest tile function that either builds a single pixel in image or maps (x,y) to mercator lat, lon, to plot
//    // TODO: write small caching to ensure tiles are not all recomputed
//    // @tailrec
////    def generateTile(tile: Tile, maxZoomLevel: Int = 4): List[Pixel] = {
////      if (tile.zoom >= maxZoomLevel) {
////        val maxX = tile.zoom << 1
////        val index = (tile.x + (tile.y * maxX)) % palette.length
////        println(s"for tile (${tile.x}, ${tile.y}) at zoom ${tile.zoom} have index $index")
////        List.fill(tile.tileSize * tile.tileSize)(PixelTools.rgb(palette(index)._1, palette(index)._2, palette(index)._3))
////      } else {
////        println(s"for tile (${tile.x}, ${tile.y}) at zoom ${tile.zoom} dividing into")
////        val northWest = generateTile(tile.NW, maxZoomLevel)
////        val northEast = generateTile(tile.NE, maxZoomLevel)
////        val southWest = generateTile(tile.SW, maxZoomLevel)
////        val southEast = generateTile(tile.SE, maxZoomLevel)
////
////        combineTiles(northWest, northEast, southWest, southEast)
////      }
////    }
//
//
////    val worldCoords = 0 until (width.toInt * width.toInt)
////    val worldColors = worldCoords.par.map { i => {
////      //    val worldColors = worldCoords.map { i => {
//////      val (rowIndex, colIndex) = indices(i)
//////      val lat = latMap(rowIndex)
//////      val lon = lonMap(colIndex)
//////      val col = Visualization.interpolateColorWithScale(tempScale,
//////        Visualization.predictTemperature(temperatures, Location(lat, lon)))
//////      Pixel(PixelTools.rgb(col.red, col.green, col.blue))
////      Pixel(PixelTools.rgb(0, 0, 0))
////    }
////    }
////    val imgArray = generateTile(tile, maxZoomLevel = 3) .toArray
////    val width = math.sqrt(imgArray.length)
////
////    val img = Image(width.toInt, width.toInt, imgArray)
//////    img.scaleTo(256, 256, FastScale)
////    img
//  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Year, Data)],
                           generateImage: (Year, Tile, Data) => Unit
                         ): Unit = {


  }

  def tileSave(year: Year, tileToSave: Tile, temperatures: Iterable[(Location, Temperature)]): Unit = {

    //TODO: change tile
//    val img = tile(temperatures, colorScale, tileToSave)
//    val dirName = s"target/temperatures/$year/${tileToSave.zoom}"
//    val pathName = s"$dirName/${tileToSave.x}-${tileToSave.y}.png"
//    val success = (new File(dirName)).mkdirs
//
//    img.output(new java.io.File(pathName))
  }
}
