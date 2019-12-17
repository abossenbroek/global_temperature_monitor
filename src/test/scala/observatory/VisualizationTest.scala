package observatory


import java.io.File

import com.sksamuel.scrimage.Position.TopLeft
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers with Matchers {

  import Visualization._

  test("Test color is properly interpolated") {
    val interMediateColor = interpolateColor(List((-1.0, Color(255, 0, 0)), (0, Color(0, 0, 255))), -0.5)
    assert(interMediateColor === Color(128, 0, 128))
  }

  test("Exceeding the color scale should give the largest color") {
    val maxColor = interpolateColor(List((0.0, Color(255, 0, 0)), (1.0, Color(0, 0, 255))), 1.5)
    assert(maxColor === Color(0, 0, 255), "Maximum color should be extrapolated")
    val minColor = interpolateColor(List((0.0, Color(255, 0, 0)), (1.0, Color(0, 0, 255))), -0.5)
    assert(minColor === Color(255, 0, 0), "Minimum color should be extrapolated")
  }

  test("More advanced color scaling") {
    val intermediateColor = interpolateColor(List((-2.147483648E9, Color(255, 0, 0)), (0.0, Color(0, 0, 255))), -1.610612736E9)
    assert(intermediateColor === Color(191, 0, 64))
  }

  test("If locations are less than 1km apart temperaturePrediction should return that temperature") {
    val temps = List((Location(48.6598091, -114.1260281), 20d), (Location(48.6598091, 114.1260281), 30d))
    val interpolTemp = predictTemperature(temps, Location(48.6598091, -114.1260281 + 110 / 111))
    assert(interpolTemp === 20)
  }

  test("predicted temperature at location z should be closer to known temperature at location x than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa") {
    val glacierLoc = Location(48.6598091, -114.1260281)
    val amsterdamLoc = Location(52.3546274, 4.8285839)
    val londonLoc = Location(51.5285582, -0.2416796)
    val temps = List((glacierLoc, -10d), (amsterdamLoc, 5d))
    val interpolTemp = predictTemperature(temps, londonLoc)
    assert((interpolTemp - temps(0)._2).abs > (interpolTemp - temps(1)._2).abs)
  }

  test("Coursera Scale and values 1") {
    val temps = List((Location(45.0, -90.0), 10.0), (Location(-45.0, 0.0), 20.0))
    val colScheme = List((10.0, Color(255, 0, 0)), (20.0, Color(0, 0, 255)))

    val img = visualize(temps, colScheme)

    img.output(new File("target/coursera_full1.png"))
  }

  test("Coursera Scale and values 2") {
    val temps = Array((Location(45.0, -90.0), 20.0), (Location(45.0, 90.0), 0.0), (Location(0.0, 0.0), 10.0),
      (Location(-45.0, -90.0), 0.0), (Location(-45.0, 90.0), 20.0))
    val colScheme = List((0.0, Color(255, 0, 0)), (10.0, Color(0, 255, 0)), (20.0, Color(0, 0, 255)))

    val img = visualize(temps, colScheme)

    img.output(new File("target/coursera_full2.png"))
  }


}
