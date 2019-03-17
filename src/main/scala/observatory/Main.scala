package observatory

import com.typesafe.scalalogging.LazyLogging
import org.apache.log4j.{Level, Logger}
//import org.scalameter._

object Main extends App with LazyLogging {

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
  Logger.getLogger("observatory").setLevel(Level.DEBUG)
  val forkJoinPool = new java.util.concurrent.ForkJoinPool(3)

  logger.info("Loading data")
 val temp1989 = Extraction.locateTemperatures(1989,
   "/stations.csv",
////   "/temperature_test.csv")
//   //"src/main/resources/stations.csv",
//  // "src/main/resources/[12]*.csv")
  "/1989.csv")
//
  logger.info("Extracting temperatures")
  val temps = Extraction.locationYearlyAverageRecords(temp1989)
//
//  val fos = new FileOutputStream("temp.tmp")
//  val oos = new ObjectOutputStream(fos)
//  oos.writeObject(temps.take(2000))
//  oos.close()

//  val fis = new FileInputStream("temp.tmp")
//  val ois = new ObjectInputStream(fis)
//  val temps = ois.readObject().asInstanceOf[Iterable[(Location, Temperature)]]

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

  import Visualization._

  println(s"Visualization using ${temps.toSeq.length}")
//  val time = config(
//    Key.exec.benchRuns -> 20,
//    Key.verbose -> false
//  ) withWarmer {
//    new Warmer.Default
//  } withMeasurer {
//    new Measurer.IgnoringGC
//  } measure {
    visualize(temps, colorScale)
//  }
//
//  println(s"Total time: $time")

    forkJoinPool.shutdown()
}
