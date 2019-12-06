package observatory

import com.sksamuel.scrimage.{Image, Pixel, PixelTools}
//import com.typesafe.scalalogging.LazyLogging
//import org.apache.commons.math3.util.{FastMath => math}

import scala.collection.immutable.TreeMap

/**
  * 2nd milestone: basic visualization
  */
//object Visualization extends LazyLogging {
object Visualization {
  val colorScale = List((-60d, Color(0, 0, 0)),
    (-50d, Color(33, 0, 107)),
    (-27d, Color(255, 0, 255)),
    (-15d, Color(0, 0, 255)),
    (0d, Color(0, 255, 255)),
    (12d, Color(255, 255, 0)),
    (32d, Color(255, 0, 0)),
    (60d, Color(255, 255, 255)))

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    // Based on https://gis.stackexchange.com/questions/142326/calculating-longitude-length-in-miles
    val kmPerDegree = 111d
    val oneOverKmPerDegree = 1 / kmPerDegree
    val oneOverKmPerDegreeSq = 1 / (kmPerDegree * kmPerDegree)
    val p = 6d

    def squaredEuclideanDistance(a: Location): Double = {
      (a.lon - location.lon) * (a.lon - location.lon) + (a.lat - location.lat) * (a.lat - location.lat)
    }

    val squaredEuclidianDistances = temperatures
      .map{case (l, t) => squaredEuclideanDistance(l) -> t}.toMap

    if (squaredEuclidianDistances.keys.min < oneOverKmPerDegreeSq) {
      squaredEuclidianDistances(squaredEuclidianDistances.keys.min)
    } else {
      val earthRadius = 6357 // km

      def greatCircle(a: Location): Double =
        if (a == location) 0
        else if ((a.lon + location.lon).abs < oneOverKmPerDegree
          && (a.lat + location.lat).abs < oneOverKmPerDegree) math.Pi
        else {
          val absDiffLon = (math.toRadians(a.lon) - math.toRadians(location.lon)).abs

          math.acos(math.sin(math.toRadians(a.lat)) * math.sin(math.toRadians(location.lat))
            + math.cos(math.toRadians(a.lat)) * math.cos(math.toRadians(location.lat)) * math.cos(absDiffLon))
        }

      def distance(a: Location) = {
        val dist = greatCircle(a)
        val corDist = if (dist.isNaN) 1 else dist
        math.pow(earthRadius * corDist, -p)
      }

      val weightsCalc = temperatures.foldLeft((0d, 0d))((acc, key) => {
        val dist = distance(key._1)
        val acc1 = acc._1 + dist * key._2
        val acc2 = acc._2 + dist
        (acc1, acc2)
      })

      weightsCalc._1 / weightsCalc._2
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    def interpolator(color1: (Temperature, Color), color2: (Temperature, Color), toInterpolate: Temperature): Color = {
      def linearInterpolation(y0: Int, y1: Int): Int = {
        val x0 = color1._1
        val x1 = color2._1

        val scale = ((toInterpolate - x0) / (x1 - x0).toFloat).toFloat
        math.round(y0 * (1f - scale) + y1 * scale)
      }

      val redComp = linearInterpolation(color1._2.red, color2._2.red)
      val greenComp = linearInterpolation(color1._2.green, color2._2.green)
      val blueComp = linearInterpolation(color1._2.blue, color2._2.blue)

      Color(redComp, greenComp, blueComp)
    }

    val scale = calculateScale(points)

    if (value >= scale.keys.max) {
      scale(scale.keys.max)
    } else if (value <= scale.keys.min) {
      scale(scale.keys.min)
    } else {
      val closest = Seq(scale.to(value).lastOption, scale.from(value).headOption)
        .flatten
        .distinct

      interpolator(closest.head, closest.tail.head, value)
    }
  }

  def calculateScale(points: Iterable[(Temperature, Color)]): TreeMap[Temperature, Color] = {
    points.toSeq
        .foldLeft(TreeMap[Temperature, Color]())((map, key) =>
          map + (key._1 -> key._2))
  }

  def interpolateColorWithScale(scale: TreeMap[Temperature, Color], value: Temperature): Color = {
    def interpolator(color1: (Temperature, Color), color2: (Temperature, Color), toInterpolate: Temperature): Color = {
      def linearInterpolation(y0: Int, y1: Int): Int = {
        val x0 = color1._1
        val x1 = color2._1

        val scale = (toInterpolate - x0) / (x1 - x0).toFloat
        (y0 * (1f - scale) + y1 * scale).round.toInt
      }

      val redComp = linearInterpolation(color1._2.red, color2._2.red)
      val greenComp = linearInterpolation(color1._2.green, color2._2.green)
      val blueComp = linearInterpolation(color1._2.blue, color2._2.blue)

      Color(redComp, greenComp, blueComp)
    }

    if (value >= scale.keys.max) {
      scale(scale.keys.max)
    } else if (value <= scale.keys.min) {
      scale(scale.keys.min)
    } else {
      interpolator(scale.to(value).lastOption.head, scale.from(value).headOption.head, value)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    // All performance checks on 500 locations:
    // BASE START:                            Total time: 6089.689545249999 ms
    // Taking out calculating the scale:      Total time: 5097.940386700001 ms
    // Taking out the square root:            Total time: 4967.327434350001 ms
    // Switching to greatCircle to FastMath:  Total time: 3724.4725129499993 ms
    // Refactor all math to FastMath:         Total time: 3534.2878032 ms
    // Removing unnecessary seq:              Total time: 3338.6400333000006 ms
    // Removing double foldLeft:              Total time: 2950.2546134500003 ms
    // Similar results after removing all pars and removing branching from foldLeft in predictTemperature

    // Bigger benchmark:
    //    Visualization using 2000
    //    Total time: 14690.48794335 ms
    //
    // After reducing the level of detail in the lat/lon we find:
    //    Visualization using 2000
    //    Total time: 13152.365002850003 ms

    val totalLatitude = 180
    val totalLongitude = 360

    val height = 180
    val width = 360

    def roundToNearest(d: Double, n: Double): Double =
      math.round(d * n)

    def reducePrecision(imgHeight: Double, imgWidth: Double,
                        toReduce: Iterable[(Location, Temperature)]): Iterable[(Location, Temperature)] = {
      val heightNearest = imgHeight / totalLatitude
      val widthNearest = imgWidth / totalLongitude

      val reduced = toReduce.par.map{case (l, t) =>
        val newLat = roundToNearest(l.lat, heightNearest)
        val newLon = roundToNearest(l.lon, widthNearest)
        ((newLat, newLon), t)
      }.toMap

      reduced.groupBy(_._1).par.map{ case (k, l) =>
        (Location(k._1 / heightNearest, k._2 / widthNearest), l.map{case (_, t) => t}.sum / l.toSeq.length)
      }.seq
    }

//    logger.debug(s"Before reduction we have ${temperatures.toSeq.length}")
    val reducedTemps = reducePrecision(height, width, temperatures)

//    logger.debug(s"Before reduction we have ${reducedTemps.toSeq.length}")

    val tempScale = calculateScale(colors)

    def worldCoordinates(i: Int): (Int, Int) = {
      val floored = math.floor(i / 360f).toInt
      val lon = i - 360 * floored - 180
      val lat = 90 - floored
      (lat, lon)}

    val worldCoords = 0 until (width * height)

    val worldColors = worldCoords.par.map{i => {
      val (lat, lon) = worldCoordinates(i)
      val col = interpolateColorWithScale(tempScale, predictTemperature(reducedTemps, Location(lat, lon)))
      Pixel(PixelTools.rgb(col.red, col.green, col.blue))
    }}
    val imgArray = worldColors.toArray

    val img = Image(width, height, imgArray)
    //img.output(new java.io.File("target/some-image.png"))
    img
  }
}

