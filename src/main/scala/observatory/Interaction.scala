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
  private val targetSquareDim = 256

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    tile.location
  }

  def latLonPixel(l: Location, z: Int): (Tile, (Int, Int)) = {
    val coordSize: Int = (math.max(1, 1 << z) * Tile(0, 0, 0).tileSize - 1)
    val y = ((l.lat - GlobalCoordinates.TopLeft.lat) /
      -(GlobalCoordinates.TopLeft.lat - GlobalCoordinates.BottomRight.lat) * coordSize).toInt
    val x = ((l.lon + GlobalCoordinates.BottomRight.lon) /
      (-GlobalCoordinates.TopLeft.lon + GlobalCoordinates.BottomRight.lon) * coordSize).toInt


    val tileX = x / Tile(0, 0, 0).tileSize
    val tileY = y / Tile(0, 0, 0).tileSize
    val inTileX = x % Tile(0, 0, 0).tileSize
    val inTileY = y % Tile(0, 0, 0).tileSize

    (Tile(tileX, tileY, zoom = z), (inTileX, inTileY))
  }


  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 pixelArray showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile):
  Image = {
    println(f"### tile: called with temperatures $temperatures")
    println(f"### tile: called with color-scheme $colors")
    println(f"### tile: called with tile $tile ")

    val tempScale = calculateScale(colors)
    val imgArray = tile.tileCoords.map{c : (Double, Double) =>
      val currentTemp = predictTemperature(temperatures, Location(c._1, c._2))
      val col = interpolateColorWithScale(tempScale, currentTemp)
      Pixel(PixelTools.argb(alpha, col.red, col.green, col.blue))
    }.toArray

    val img = Image(tileWidth.toInt, tileWidth.toInt, imgArray)
    img.scaleTo(targetSquareDim, targetSquareDim, FastScale)
  }


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
        val tiles = for (z <- 0 to 3 ; dim = (1 << z) ; x <- 0 until dim ; y <- 0 until dim) yield Tile(x, y, z)
        tiles.par.foreach {
          t => generateImage(yd._1, t, yd._2)
        }
      }
    }
  }

  def tileSave(year: Year, tileToSave: Tile, temperatures: Iterable[(Location, Temperature)]): Unit = {
    val dirName = s"target/temperatures/$year/${tileToSave.zoom}"
    val pathName = s"$dirName/${tileToSave.x}-${tileToSave.y}.png"
    new File(dirName).mkdirs
    val image = tile(temperatures, colorScale, tileToSave)
    image.output(new java.io.File(pathName))
  }
}
