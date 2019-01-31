package observatory

object Main extends App {
  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

 val temp1989 = Extraction.locateTemperatures(1989,
   "src/main/resources/stations.csv",
  // "src/main/resources/[12]*.csv")
  "src/main/resources/198[89].csv")

  println(temp1989.head)
  Extraction.locationYearlyAverageRecords(temp1989)
}
