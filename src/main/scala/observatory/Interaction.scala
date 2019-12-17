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
  val rootTileImage = TileImage(Tile(0, 0, 0))
  private val onePixelImage = Image(1, 1, Array[Pixel](PixelTools.rgb(0, 0, 0)))
  private val targetSquareDim = 256

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    tile.location
  }

  def combineTiles(NW: Array[Pixel], NE: Array[Pixel], SW: Array[Pixel], SE: Array[Pixel]): Array[Pixel] = {
    def combineCubes(a: Array[Pixel], b: Array[Pixel]): Array[Pixel] = {
      val height = math.sqrt(a.length).toInt
      Range(0, height).map { i => {
        val start = i * height
        val end = (i + 1) * height
        a.slice(start, end) ++ b.slice(start, end)
      }
      }.par.foldLeft(List[Pixel]())(_ ++ _).toArray
    }

    val top = combineCubes(NW, NE)
    val bottom = combineCubes(SW, SE)
    top ++ bottom
  }

  case class InvalidTiles() extends Exception()

  case class InvalidTree() extends Exception()

  case class ImageNotPresent() extends Exception()

  def tileImageWithChildren(t: Tile, NW: TileImage, NE: TileImage, SW: TileImage, SE: TileImage): TileImage =
    new TileImage(t, None, None, Some(NW), Some(NE), Some(SW), Some(SE))

  def tempsApplicableToTile(ti: TileImage, temperatures: Iterable[(Location, Temperature)]):
  Iterable[(Location, Temperature)] =
    temperatures.filter { temp => (ti.location >~= temp._1) && temp._1 > ti.bottomRight }

  case class TileImage(t: Tile,
                       pixelArray: Option[Array[Pixel]],
                       image: Option[Image],
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
    lazy val hasImage = image.isDefined


    def depth: Int = (NW, NE, SW, SE) match {
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

    def tileCoordinate(i: Int): Location = {
      val (lat, lon) = indices(i)
      Location(latMap(lat), lonMap(lon))
    }

    def visualize(temps: => Iterable[(Location, Temperature)]): TileImage = {
      visualize(temps, colorScale)
    }

    def visualize(temps: => Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): TileImage = {
      val tempScale = calculateScale(colors)
      visualizeWorker(temps, tempScale)
    }

    def visualizeWorker(temps: => Iterable[(Location, Temperature)], tempScale: => TreeMap[Temperature, Color]):
    TileImage = (NW, NE, SW, SE) match {
      case (Some(nw), Some(ne), Some(sw), Some(se)) =>
        val newNW = nw.visualizeWorker(temps, tempScale)
        val newNE = ne.visualizeWorker(temps, tempScale)
        val newSW = sw.visualizeWorker(temps, tempScale)
        val newSE = se.visualizeWorker(temps, tempScale)

        def getImage(c: TileImage): Array[Pixel] = c.pixelArray.getOrElse(Array[Pixel]())

        val newPixelArray = combineTiles(getImage(newNW), getImage(newNE), getImage(newSW), getImage(newSE))
        val width = newNW.image.getOrElse(onePixelImage).width * 2
        val image = Image(width, width, newPixelArray).scaleTo(targetSquareDim, targetSquareDim, FastScale)

        new TileImage(t, Some(image.pixels), Some(image), Some(newNW), Some(newNE), Some(newSW), Some(newSE))
      case _ =>
        val tileCoords = 0 until (tileWidth * tileWidth).toInt
        val tileColors = tileCoords.par.map { i => {
          val currentLocation = tileCoordinate(i)
          val currentTemp = predictTemperature(temps, currentLocation)
          val col = interpolateColorWithScale(tempScale, currentTemp)
          Pixel(PixelTools.argb(alpha, col.red, col.green, col.blue))
        }
        }
        val imgArray = tileColors.toArray
        val img = Image(tileWidth.toInt, tileWidth.toInt, imgArray)
        val scaledImg = img.scaleTo(targetSquareDim, targetSquareDim, FastScale)
        this.copy(pixelArray = Some(scaledImg.pixels), image = Some(scaledImg))
    }

    def getTiles: List[Tile] = (NW, NE, SW, SE) match {
      case (Some(nw), Some(ne), Some(sw), Some(se)) =>
        t :: List(nw, ne, sw, se).foldLeft(List[Tile]())(_ ++ _.getTiles)
      case (_, _, _, _) =>
        List[Tile](t)
    }


    def save(year: Int): Unit = {
      if (image.isEmpty) throw ImageNotPresent()

      def saveImage(): Unit = {
        val dirName = s"target/temperatures/$year/$zoom"
        val pathName = s"$dirName/${t.x}-${t.y}.png"
        new File(dirName).mkdirs
        image.getOrElse(onePixelImage).output(new java.io.File(pathName))
      }

      (NW, NE, SW, SE) match {
        case (Some(nw), Some(ne), Some(sw), Some(se)) =>
          List(nw, ne, sw, se).foreach {
            _.save(year)
          }
        case (_, _, _, _) =>
      }

      saveImage
    }

    def getTileImage(target: Tile): Option[TileImage] = {
      if (target == t) Some(this)
      else (NW, NE, SW, SE) match {
        case (Some(nw), Some(ne), Some(sw), Some(se)) =>
          val tiles = List(nw, ne, sw, se)
          tiles.foreach { ti =>
            val res = ti.getTileImage(target)
            if (res.nonEmpty) res
          }
          None
        case (_, _, _, _) =>
          None
      }
    }

    def grow(levels: Int): TileImage = (levels, NW, NE, SW, SE) match {
      case (0, _, _, _, _) => this
      case (i, None, None, None, None) =>
        val newLevels = i - 1
        val NW = TileImage(Tile(t.x * 2, t.y * 2, t.zoom + 1)).grow(newLevels)
        val NE = TileImage(Tile(t.x * 2 + 1, t.y * 2, t.zoom + 1)).grow(newLevels)
        val SW = TileImage(Tile(t.x * 2, t.y * 2 + 1, t.zoom + 1)).grow(newLevels)
        val SE = TileImage(Tile(t.x * 2 + 1, t.y * 2 + 1, t.zoom + 1)).grow(newLevels)
        tileImageWithChildren(t, NW, NE, SW, SE)
      case (i, Some(nw), Some(ne), Some(sw), Some(se)) =>
        val newLevels = i - 1
        val NW = nw.grow(newLevels)
        val NE = ne.grow(newLevels)
        val SW = sw.grow(newLevels)
        val SE = se.grow(newLevels)
        tileImageWithChildren(t, NW, NE, SW, SE)
      case _ => throw new InvalidTree
    }
  }

  object TileImage {
    def apply(t: Tile): TileImage = {
      new TileImage(t, None, None, None, None, None, None)
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
    * @return A 256×256 pixelArray showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile):
  Image = {
    val rootNode = rootTileImage.grow(levels = tile.zoom)
    val visTree = rootNode.visualize(temperatures, colors)
    println(f"### tile: called with temperatures $temperatures")
    println(f"### tile: called with color-scheme $colors")
    println(f"### tile: searching for tile $tile and finding ${visTree.getTileImage(tile).getOrElse(rootNode).t}")
    visTree.getTileImage(tile).getOrElse(rootTileImage).image.getOrElse(onePixelImage)
  }

  // TODO: add max zoom level to map
  val tileImageByYear: collection.concurrent.Map[Year, Int] = collection.concurrent.TrieMap.empty[Year, Int]

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
    println(s"### generateTiles: ${yearlyData.toList.take(20)}")
    yearlyData.groupBy(yd => yd._1).foreach { yD =>
      yD._2.foreach { yd =>
        print(s"-- working on ${yd._1} ${yd._2}")
        val tiles = rootTileImage.grow(3).getTiles
        tiles.foreach {
          t => generateImage(yd._1, t, yd._2)
        }
      }
    }
  }

  def tileSave(year: Year, tileToSave: Tile, temperatures: Iterable[(Location, Temperature)]): Unit = {
    val defaultLevel = 3
    def generateTile: Unit = {
      val zoomLevel = math.max(defaultLevel, tileToSave.zoom)
      val newEntry = rootTileImage.grow(zoomLevel).visualize(temperatures)
      newEntry.save(year)
    }

    if (tileImageByYear.getOrElse(year, -1) < tileToSave.zoom) {
      generateTile
      tileImageByYear.update(year, defaultLevel)
    }
  }
}
