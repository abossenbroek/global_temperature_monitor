package observatory

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

//import com.typesafe.scalalogging.LazyLogging
import org.apache.log4j.{Level, Logger}
//import org.scalameter._

//object Main extends App with LazyLogging {
object Main extends App {
  import Interaction._

  val generateTemp = false
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
  val forkJoinPool = new java.util.concurrent.ForkJoinPool(6)

  if (generateTemp) {
//    logger.info("Loading data")
    val temp1989 = Extraction.locateTemperatures(1989,
      "/stations.csv",
      ////   "/temperature_test.csv")
      //   //"src/main/resources/stations.csv",
      //  // "src/main/resources/[12]*.csv")
      "/1975.csv")

//    logger.info("Extracting temperatures")
    val temps = Extraction.locationYearlyAverageRecords(temp1989)

    val fos = new FileOutputStream("temp.tmp")
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(temps.take(2000))
    oos.close()
  }

  val fis = new FileInputStream("temp.tmp")
  val ois = new ObjectInputStream(fis)
  val temps = ois.readObject().asInstanceOf[Iterable[(Location, Temperature)]]

  println(temps.take(10))

////  logger.info(s"Visualization using ${temps.toSeq.length}")
////  val time = config(
////    Key.exec.benchRuns -> 20,
////    Key.verbose -> false
////  ) withWarmer {
////    new Warmer.Default
////  } withMeasurer {
////    new Measurer.IgnoringGC
////  } measure {
////    visualize(temps, colorScale)
////  }
////
////  println(s"Total time: $time")
  generateTiles(List((1989, temps.take(10))), tileSave)

  forkJoinPool.shutdown()
}
