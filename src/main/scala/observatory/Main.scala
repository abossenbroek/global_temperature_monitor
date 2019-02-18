package observatory

object Main extends App {
  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
  val forkJoinPool = new java.util.concurrent.ForkJoinPool(6)
//
  println("Loading data")
 val temp1989 = Extraction.locateTemperatures(1989,
   "/stations.csv",
////   "/temperature_test.csv")
//   //"src/main/resources/stations.csv",
//  // "src/main/resources/[12]*.csv")
  "/1989.csv")
//
  println("Extracting temperatures")
  val temps = Extraction.locationYearlyAverageRecords(temp1989)

  import Visualization._
//  val glacierLoc = Location(48.6598091, -114.1260281)
//  val amsterdamLoc = Location(52.3546274, 4.8285839)
//  val londonLoc = Location(51.5285582, -0.2416796)
//  val temps = List((glacierLoc, -10d), (amsterdamLoc, 5d), (londonLoc, 10d))

  val colorScale = List((-60d, Color(0, 0, 0)),
    (-50d, Color(33, 0, 107)),
    (-27d, Color(255, 0, 255)),
    (-15d, Color(0, 0, 255)),
    (0d, Color(0, 255, 255)),
    (12d, Color(255, 255, 0)),
    (32d, Color(255, 0, 0)),
    (60d, Color(255, 255, 255)))

  println("Visualization")
  visualize(temps, colorScale)

  forkJoinPool.shutdown()
}
