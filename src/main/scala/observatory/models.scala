package observatory

/**
  * Introduced in Week 1. Represents a location on the globe.
  *
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double) {
  def myTile(zoom: Int): Tile = {
    val n: Double = 1 << zoom
    val xTile = (lon + 180d) / 360d * n
    val latRad = math.toRadians(lat)
    val yTile = (1d - math.log(math.tan(latRad) + (1 / math.cos(latRad))) / math.Pi) / 2.0 * n

    Tile(xTile.toInt, yTile.toInt, zoom)
  }
}

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  * @param x X coordinate of the tile
  * @param y Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int) {
  val tileSize: Int = 256
  lazy val numTiles: Double = 1 << zoom
  lazy val location: Location = Location(math.toDegrees(math.atan(math.sinh(math.Pi * (1d - 2d * y / numTiles)))),
    x.toDouble / numTiles * 360d - 180d)

  private lazy val newNorth = y * 2
  private lazy val newSouth = y * 2 + 1
  private lazy val newWest = x * 2
  private lazy val newEast = x * 2 + 1
  lazy val subTiles: (Tile, Tile, Tile, Tile) = (Tile(newWest, newNorth, zoom + 1),
    Tile(newEast, newNorth, zoom + 1),
    Tile(newWest, newSouth , zoom + 1),
    Tile(newEast, newSouth, zoom + 1)
  )

//  def fromPixelToLocation(pixel: (Int, Int)): Location =
//    Location(math.toDegrees(math.atan(math.sinh(math.Pi * (1d - 2d * pixel._2.toDouble / imgSize.toDouble)))),
//      pixel._1.toDouble / imgSize.toDouble * 360d - 180d)

  // TODO: consider refactoring image conversion to here
}

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int)

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double)

/**
  * Introduced in Week 2. Represents an RGB color.
  * @param red Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int)

