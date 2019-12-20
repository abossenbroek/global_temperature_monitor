package observatory

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

//import com.typesafe.scalalogging.LazyLogging
import org.apache.log4j.{Level, Logger}
//import org.scalameter._

//object Main extends App with LazyLogging {
object Main extends App {
  import Interaction._
//object Main extends App {
  val cacheFileName = "temp.tmp"
  val generateTemp = new java.io.File(cacheFileName).exists
  println(s"Cache file $cacheFileName was found: $generateTemp")

  val forkJoinPool = new java.util.concurrent.ForkJoinPool(6)

  if (!generateTemp) {
//    logger.info("Loading data")
    val temp1989 = Extraction.locateTemperatures(1989,
      "/stations.csv",
      ////   "/temperature_test.csv")
      //   //"src/main/resources/stations.csv",
      //  // "src/main/resources/[12]*.csv")
      "/1975.csv")

//    logger.info("Extracting temperatures")
    val temps = Extraction.locationYearlyAverageRecords(temp1989)

    val fos = new FileOutputStream(cacheFileName)
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(temps.take(2000))
    oos.close()
  }

//  val fis = new FileInputStream(cacheFileName)
//  val ois = new ObjectInputStream(fis)
//  val temps = ois.readObject().asInstanceOf[Iterable[(Location, Temperature)]]
//
//  println(temps.take(10))

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
//  generateTiles(List((1989, temps.take(10))), tileSave)

  val t = Tile(0, 0, 0)
  println(s"latmap ${t.latMap}")
  println(s"lonmap ${t.lonMap}")

//  val t1 = Tile(2, 5, 3)
//  val t2 = Tile(3, 6, 3)
//  println(s"latMap $t1 \t: ${t1.latMap}")
//  println(s"latMap $t2 \t: ${t2.latMap}")
//
//  val t3 = Tile(1, 2, 2)
//  val t4 = Tile(2, 3, 2)
//  println(s"lonMap $t3 \t: ${t1.lonMap}")
//  println(s"lonMap $t4 \t: ${t2.lonMap}")

  (0 until 8).map { x =>
    val t = Tile(x, 0, 3)
    println(s"for $t\t\t latMap: ${t.lonMap}")
  }

  forkJoinPool.shutdown()
}
