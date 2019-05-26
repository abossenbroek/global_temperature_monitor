package observatory

import java.io.File

import com.sksamuel.scrimage.{Image, Pixel, PixelTools}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    tile.location
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val width = 256f
//    val width = 128f
    val alpha = 70
    val latIdx = tile.latSpan / width
    val lonIdx = tile.lonSpan / width.toFloat

    // TODO: add check to map indices back to proper lat lon with slippy map tilenames
    // Check possible use of https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Subtiles
    def indices(i: Int): (Int, Int) = {
      val rowNum = math.floor(i / width)
      val colNum = i - width * rowNum
      (rowNum.toInt, colNum.toInt)
    }

    val latMap = (0 until width.toInt).map { i =>
      tile.location.lat + latIdx * i
    }

    val lonMap = (0 until width.toInt).map { i =>
      tile.location.lon + lonIdx * i
    }

    val tempScale = Visualization.calculateScale(colors)

    val worldCoords = 0 until (width.toInt * width.toInt)
    val worldColors = worldCoords.par.map { i => {
//    val worldColors = worldCoords.map { i => {
      val (rowIndex, colIndex) = indices(i)
      val lat = latMap(rowIndex)
      val lon = lonMap(colIndex)
      val col = Visualization.interpolateColorWithScale(tempScale,
        Visualization.predictTemperature(temperatures, Location(lat, lon)))
      Pixel(PixelTools.rgb(col.red, col.green, col.blue))
    }
    }
    val imgArray = worldColors.toArray

    val img = Image(width.toInt, width.toInt, imgArray)
//    img.scaleTo(256, 256, FastScale)
    img
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

    def genMap(zoom: Int, year: Year, data: Data): Unit = {
      val xTiles = math.pow(2, zoom).toInt
      val totalTiles = xTiles * xTiles

      def indices(i: Int): (Int, Int) = {
        val rowNum = math.floor(i / xTiles.toFloat).toInt
        val colNum = i - xTiles * rowNum
        (rowNum, colNum)
      }

      (0 until totalTiles).foreach { i =>
        val (rowIdx, colIdx) = indices(i)
        generateImage(year, Tile(colIdx, rowIdx, zoom), data)
      }
    }

    val maps = yearlyData.flatMap { i =>
      (0 to 3).map { z =>
        Future {
          genMap(z, i._1, i._2)
        }
      }
    }

    maps.foreach(f => Await.result(f, 5000 second))

    //    for {
    //      (y, d) <- yearlyData
    //      z <- 1 to 3
    //      //f <- Future(genMap(z, y, d))
    //    }(genMap(z, y, d))

  }

  def tileSave(year: Year, tileToSave: Tile, temperatures: Iterable[(Location, Temperature)]): Unit = {
    val colorScale = List((-60d, Color(0, 0, 0)),
      (-50d, Color(33, 0, 107)),
      (-27d, Color(255, 0, 255)),
      (-15d, Color(0, 0, 255)),
      (0d, Color(0, 255, 255)),
      (12d, Color(255, 255, 0)),
      (32d, Color(255, 0, 0)),
      (60d, Color(255, 255, 255)))

    val img = tile(temperatures, colorScale, tileToSave)
    val dirName = s"target/temperatures/$year/${tileToSave.zoom}"
    val pathName = s"$dirName/${tileToSave.x}-${tileToSave.y}.png"
    val success = (new File(dirName)).mkdirs

    img.output(new java.io.File(pathName))
  }
}
