package observatory

import java.io.File

import com.sksamuel.scrimage.{Image, Pixel, PixelTools}

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

  def newSlippyTileCoord(tile: Tile): (Int, Int, Int, Int) = {
    val newNorth = tile.y * 2
    val newSouth = tile.y * 2 + 1
    val newWest = tile.x * 2
    val newEast = tile.x * 2 + 1
    (newNorth, newSouth, newWest, newEast)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
     // Tile can be split up in recursion, results can be merged back together

//    val width = 256f
//    val width = 128f
    val alpha = 70
//    val latIdx = tile.latSpan / width
//    val lonIdx = tile.lonSpan / width

    // TODO:
    // 1. Determine left and right most x
    // 2. Determine top and bottom most y


    // TODO: add check to map indices back to proper lat lon with slippy map tilenames
    // Check possible use of https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Subtiles
//    def indices(i: Int): (Int, Int) = {
//      val rowNum = math.floor(i / width)
//      val colNum = i - width * rowNum
//      (rowNum.toInt, colNum.toInt)
//    }
//
//    // TODO: these are pure x, y pixels values and not mercator mapping
//    val latMap = (0 until width.toInt).map { i =>
//      tile.location.lat + latIdx * i
//    }
//
//    val lonMap = (0 until width.toInt).map { i =>
//      tile.location.lon + lonIdx * i
//    }
//
    //    val tempScale = Visualization.calculateScale(colors)

    val palette = List[(Int, Int, Int)](
      (0,0,0),
      (87, 87, 87),
      (173,35,35),
      (42, 75, 215),
      (29, 105, 20),
      (129,74,25),
      (129,38,192),
      (160,160,160),
      (129,197,122),
      (157,175,255),
      (41,208,208),
      (255,146,51),
      (255,238,51),
      (233,222,187),
      (255,205,243),
      (255,255,255)
    )
    // TODO: write recursive function that builds tile from smaller tiles
    // TODO: write smallest tile function that either builds a single pixel in image or maps (x,y) to mercator lat, lon, to plot
    // TODO: write small caching to ensure tiles are not all recomputed
    // @tailrec
    def generateTile(tile: Tile, maxZoomLevel: Int = 4): List[Pixel] = {
      if (tile.zoom >= maxZoomLevel) {
        val maxX = tile.zoom << 1
        val index = (tile.x + (tile.y * maxX)) % palette.length
        println(s"for tile (${tile.x}, ${tile.y}) at zoom ${tile.zoom} have index $index")
        List.fill(tile.tileSize * tile.tileSize)(PixelTools.rgb(palette(index)._1, palette(index)._2, palette(index)._3))
      } else {
        val (newNorth, newSouth, newWest, newEast) = newSlippyTileCoord(tile)

        println(s"for tile (${tile.x}, ${tile.y}) at zoom ${tile.zoom} dividing into ($newNorth, $newWest) ($newNorth, $newEast), ($newSouth, $newWest), ($newSouth, $newEast)")
        val northWest = generateTile(Tile(newNorth, newWest, tile.zoom + 1), maxZoomLevel=maxZoomLevel)
        val northEast = generateTile(Tile(newNorth, newEast, tile.zoom + 1), maxZoomLevel=maxZoomLevel)
        val southWest = generateTile(Tile(newSouth, newWest, tile.zoom + 1), maxZoomLevel=maxZoomLevel)
        val southEast = generateTile(Tile(newSouth, newEast, tile.zoom + 1), maxZoomLevel=maxZoomLevel)

        def combineCubes(a: List[Pixel], b: List[Pixel]): List[Pixel] = {
          val height = math.sqrt(a.length).toInt
          Range(0, height).map{i => {
            val start = i * height
            val end = (i + 1) * height
            a.slice(start, end) ++ b.slice(start, end)
          }
          }.foldLeft(List[Pixel]())(_++_)
          }

        val top = combineCubes(northWest, northEast)
        val bottom = combineCubes(southWest, southEast)
       top ++ bottom
      }
    }


//    val worldCoords = 0 until (width.toInt * width.toInt)
//    val worldColors = worldCoords.par.map { i => {
//      //    val worldColors = worldCoords.map { i => {
////      val (rowIndex, colIndex) = indices(i)
////      val lat = latMap(rowIndex)
////      val lon = lonMap(colIndex)
////      val col = Visualization.interpolateColorWithScale(tempScale,
////        Visualization.predictTemperature(temperatures, Location(lat, lon)))
////      Pixel(PixelTools.rgb(col.red, col.green, col.blue))
//      Pixel(PixelTools.rgb(0, 0, 0))
//    }
//    }
    val imgArray = generateTile(tile, maxZoomLevel = 2) .toArray
    val width = math.sqrt(imgArray.length)

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
//      val xTiles = math.pow(2, zoom).toInt
//      val totalTiles = xTiles * xTiles
//
//      def indices(i: Int): (Int, Int) = {
//        val rowNum = math.floor(i / xTiles.toFloat).toInt
//        val colNum = i - xTiles * rowNum
//        (rowNum, colNum)
//      }

//      (0 until totalTiles).foreach { i =>
//        val (rowIdx, colIdx) = indices(i)
        generateImage(year, Tile(0, 0, 0), data)
//      }
    }
//
//
    yearlyData.map { i =>
//      (0 to 2).map { z =>
          genMap(1, i._1, i._2)
//      }
    }

//    val maps = yearlyData.flatMap { i =>
//      (0 to 3).map { z =>
//        Future {
//          genMap(z, i._1, i._2)
//        }
//      }
//    }
//
//    maps.foreach(f => Await.result(f, 5000 second))

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

    //TODO: change tile
    val img = tile(temperatures, colorScale, Tile(0,0,0))
    val dirName = s"target/temperatures/$year/${tileToSave.zoom}"
    val pathName = s"$dirName/${tileToSave.x}-${tileToSave.y}.png"
    val success = (new File(dirName)).mkdirs

    img.output(new java.io.File(pathName))
  }
}
