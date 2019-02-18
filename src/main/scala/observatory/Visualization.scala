package observatory

import com.sksamuel.scrimage.{Image, Pixel, PixelTools}

import scala.collection.immutable.TreeMap

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    // Based on https://gis.stackexchange.com/questions/142326/calculating-longitude-length-in-miles
    val kmPerDegree = 111d

    def euclideanDistance(a: Location): Double = {
      math.sqrt((a.lon - location.lon) * (a.lon - location.lon) + (a.lat - location.lat) * (a.lat - location.lat))
    }

    val euclidianDistances = temperatures
      .par
      .foldLeft(Map[Double, Temperature]())((map, key) => map + (euclideanDistance(key._1) -> key._2))

    if (euclidianDistances.keys.min < 1 / kmPerDegree) {
      euclidianDistances(euclidianDistances.keys.min)
    } else {
      val earthRadius = 6357 // km

      def greatCircle(a: Location): Double =
        if (a == location) 0
        else if ((a.lon + location.lon).abs < 1 / kmPerDegree && (a.lat + location.lat).abs < 1 / kmPerDegree) math.Pi
        else {
          val absDiffLon = (math.toRadians(a.lon) - math.toRadians(location.lon)).abs

          math.acos(math.sin(math.toRadians(a.lat)) * math.sin(math.toRadians(location.lat))
            + math.cos(math.toRadians(a.lat)) * math.cos(math.toRadians(location.lat)) * math.cos(absDiffLon))
        }

//      assert(greatCircle(location) < 1e-6)
//      assert((greatCircle(Location(-location.lat, -location.lon)) - math.Pi).abs < 1e-6)
//      assert((greatCircle(Location(location.lat, -location.lon)) - math.Pi).abs > 1e-6)

      def distance(a: Location, p: Double = 1.5) = math.pow(earthRadius * greatCircle(a), -p)

      val weights = temperatures.par.foldLeft(Map[Double, Temperature]())((map, key) => map + (distance(key._1) -> key._2))

      val weigthNumerator = weights.par.foldLeft(0d)((acc, key) => acc + key._1 * key._2)
      val weigthDenum = weights.par.foldLeft(0d)((acc, key) => acc + key._1)

      weigthNumerator / weigthDenum
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

        val scale = (toInterpolate - x0) / (x1 - x0).toFloat
        (y0 * (1f - scale) + y1 * scale).round.toInt
      }

      val redComp = linearInterpolation(color1._2.red, color2._2.red)
      val greenComp = linearInterpolation(color1._2.green, color2._2.green)
      val blueComp = linearInterpolation(color1._2.blue, color2._2.blue)

      Color(redComp, greenComp, blueComp)
    }

    val scale = points.toSeq.par
        .foldLeft(TreeMap[Temperature, Color]())((map, key) =>
          map + (key._1 -> key._2))

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

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val width = 360
    val height = 180

//    val parallelTime = config(
//      Key.exec.benchRuns -> 10,
//      Key.verbose -> true
//    ) withWarmer(new scalameter.Warmer.Default) measure {
//      val worldCoords = for{lon <- -89 to 90; lat <- -180 to 179}
//        yield {(lon, lat)}
//      val worldColors = (worldCoords.indices zip worldCoords).par.toMap.mapValues{case(lat, lon) => {
//        val col = interpolateColor(colors, predictTemperature(temperatures, Location(lat, lon)))
//        Pixel(PixelTools.rgb(col.red, col.green, col.blue))
//      }}
//      val imgArray = worldColors.toVector.sortBy(_._1).map{_._2}
//    }
//
//    println(s"For parallel approach found time\t\t$parallelTime")
//
//    val naiveTime = config(
//      Key.exec.benchRuns -> 10,
//      Key.verbose -> true
//    ) withWarmer(new scalameter.Warmer.Default) measure {
      val interpolated = (for {lon <- -89 to 90; lat <- -180 to 179}
        yield {
          val col = interpolateColor(colors, predictTemperature(temperatures, Location(lat, lon)))
          Pixel(PixelTools.rgb(col.red, col.green, col.blue))
        }).toArray
//    }
//    println(s"For naive approach found time\t\t$naiveTime")



    //Image(width, height, Array(Pixel(0), Pixel(0)))
    Image(width, height, interpolated)
  }

}

