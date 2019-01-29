package observatory

import java.time.LocalDate

import scala.reflect.runtime.universe.TypeTag // used to provide type information of the case class at runtime
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.sql.catalyst.ScalaReflection // used to generate the schema from a case class

/**
  * 1st milestone: data extraction
  */
object Extraction {

  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Global temperature loader")
      .config("spark.master", "local")
      .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  /**
    * Determines the schema for a particular class.
    *
    * See also https://stackoverflow.com/questions/49260638/apache-spark-generic-method-for-loading-csv-data-to-dataset
    *
    * @tparam T The class type for which a schema should be defined
    * @return StrucType that allows to load data in the given class
    */
  def schemaOf[T: TypeTag]: StructType = {
    ScalaReflection
      .schemaFor[T] // this method requires a TypeTag for T
      .dataType
      .asInstanceOf[StructType] // cast it to a StructType, what spark requires as its Schema
  }

  /**
    * Read a csv into a specific class
    *
    * @param filePattern  What files should be read
    * @tparam T           The class that should be used to incur the schema
    * @return A dataset with the loaded data
    */
  def readCsv[T: Encoder: TypeTag](filePattern: String): Dataset[T] = {
    spark.read
        .schema(schemaOf[T])
      .option("header", "false")
      .csv(filePattern)
      .as[T]
  }

  case class StationData(
    stnIden: Long,
    wbanIden: Long,
    lat: Double,
    lon: Double
  )

  case class TemperatureData(
    stnIden: Long,
    wbanIden: Long,
    month: Int,
    day: Int,
    temp: Temperature
  )


  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stationData = readCsv[StationData](stationsFile)

    spark.udf.register("file_name_to_year", (path: String) => path.split("/").last.split("\\.").head.toInt)
    val temperatureData = readCsv[TemperatureData](temperaturesFile).
      withColumn("year", callUDF("file_name_to_year", input_file_name()))

    val cleanedStationData = stationData.filter($"lat".isNotNull && $"lon".isNotNull)

    // TODO: 0. aggregate station list with max of stn, wban, lat, lon to ensure that only a single entry exists
    // TODO: 1. create set of stations that have only stn identifier
    // TODO: 2. create set of stations that have only wban identifier
    // TODO: 3. create set of stations that have both identifiers (make sure to remove duplicates)
    // TODO: 4. join with temperature data
    // TODO: 5. push stationdata to lazy evaluation function
    // TODO: 6. push temperature data to lazy evaluation function

    cleanedStationData.show()
    Seq((LocalDate.of(1980,11,20), Location(0f, 0f), 1f))
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    ???
  }

}
