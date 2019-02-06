package observatory

import java.nio.file.Paths
import java.sql.Date
import java.time.LocalDate

import org.apache.spark.sql._
import org.apache.spark.sql.catalyst.ScalaReflection
import org.apache.spark.sql.types._

import scala.reflect.runtime.universe.TypeTag

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
      .config("spark.driver.memory", "1600m")
      .config("spark.executor.memory", "1600m")
      .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  /** @return The filesystem path of the given resource */
  def fsPath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString

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
    * @param filePattern What files should be read
    * @tparam T The class that should be used to incur the schema
    * @return A dataset with the loaded data
    */
  def readCsv[T: Encoder : TypeTag](filePattern: String): Dataset[T] = {
    spark.read
      .schema(schemaOf[T])
      .option("header", "false")
      .csv(fsPath(filePattern))
      .as[T]
  }

  /**
    * Raw data on a station.
    *
    * @param stnIden  a station identifier
    * @param wbanIden a second station identifier
    * @param lat      latitude of the station
    * @param lon      longitude of the station
    */
  case class StationData(
                          stnIden: Option[Long],
                          wbanIden: Option[Long],
                          lat: Double,
                          lon: Double
                        )

  /**
    * Temperature data
    *
    * @param stnIden  Station identifier for measurement
    * @param wbanIden Second station identifier for measurement
    * @param month    month when the measurement was recorded
    * @param day      day when the measurement was recorded
    * @param temp     temperature in fahrenheit
    */
  case class TemperatureData(
                              stnIden: Option[Long],
                              wbanIden: Option[Long],
                              month: Int,
                              day: Int,
                              temp: Temperature
                            )

  /**
    * Temperature measurement with year
    *
    * @param stnIden  Station identifier for measurement
    * @param wbanIden Second station identifier for measurement
    * @param month    month when the measurement was recorded
    * @param day      day when the measurement was recorded
    * @param temp     temperature in fahrenheit
    * @param year     year of measurement
    */
  case class TemperatureYearData(
                                  stnIden: Option[Long],
                                  wbanIden: Option[Long],
                                  month: Int,
                                  day: Int,
                                  temp: Temperature,
                                  year: Int
                                )

  /**
    * The raw data for a measurement
    *
    * @param lat   latitude where the measurement was made
    * @param lon   longitude where the measurement was made
    * @param month month of the measurement
    * @param day   day of measurement
    * @param temp  temperature measured in celsius
    * @param year  year of measurement
    */
  case class RawMeasurement(
                             lat: Double,
                             lon: Double,
                             month: Int,
                             day: Int,
                             temp: Temperature,
                             year: Int
                           )

  /**
    * Final format of measurement used throughout Spark data sets
    *
    * @param date java.sql.date for the measurement
    * @param lat  latitude of the measurement station
    * @param lon  longitude of the measurement station
    * @param temp temperature measured in celsius
    */
  case class SparkMeasurement(
                               date: Date,
                               lat: Double,
                               lon: Double,
                               temp: Temperature
                             )

  /**
    * Load the station data, remove stations that contain latitude or longitude that are null and sort results on
    * stnIden.
    *
    * @param stationsFile The csv file that contains station information
    * @return Dataset of station information
    */
  def loadStationData(stationsFile: String): Dataset[StationData] = {
    import org.apache.spark.sql.expressions.scalalang.typed._

    val stationData = readCsv[StationData](stationsFile)
    val cleanedStationData = stationData.filter($"lat".isNotNull && $"lon".isNotNull)

    cleanedStationData.groupByKey(row => (row.stnIden, row.wbanIden))
      .agg(
        avg(_.lat),
        avg(_.lon))
      .map { case ((stnIden, wbanIden), lat, lon) =>
        StationData(stnIden, wbanIden, lat, lon)
      }.orderBy('stnIden)
  }

  /**
    * Load the temperature data for a certain year. Data is aggregated by station identifier, year, month, date to
    * ensure daily averages are calculated.
    *
    * @param year             The year for which data should be selected
    * @param temperaturesFile The csv files that should be loaded for temperature measurements
    * @return A dataset of temperature measurements where temperatures are measured in Celsius
    */
  def loadTemperatures(year: Year, temperaturesFile: String): Dataset[TemperatureYearData] = {
    import org.apache.spark.sql.expressions.scalalang.typed._

    def f2c(f: Double): Double = (f - 32f) * 5f / 9f

    val temperatureData = readCsv[TemperatureData](temperaturesFile)
      .withColumn("year", lit(year))
      .as[TemperatureYearData]
      .repartition(12)

    temperatureData.groupByKey(row => (row.stnIden, row.wbanIden, row.month, row.day, row.year))
      .agg(
        avg(_.temp)
      )
      .map { case ((stnIden, wbanIden, month, day, y), temperature) =>
      TemperatureYearData(stnIden, wbanIden, month, day, f2c(temperature), y)
    }.orderBy('stnIden)
  }

  /**
    * Read measurements and create a dataset where the information has been reduced to data types supported by
    * Spark dataset.
    *
    * @param year             The year for which data should be loaded.
    * @param stationsFile     The file that contains data on the stations
    * @param temperaturesFile The file(s) that contain(s) data on the temperature (in Fahrenheit)
    * @return Dataset of temperature measurements with lat, lon of each measurement and temperature in Celsius
    */
  def readMeasurements(year: Year, stationsFile: String, temperaturesFile: String): Dataset[SparkMeasurement] = {
    val stationDs = loadStationData(stationsFile).na.fill(0, Seq("stnIden"))
      .na.fill(0, Seq("wbanIden"))
    val tempDs = loadTemperatures(year, temperaturesFile).na.fill(0, Seq("stnIden"))
      .na.fill(0, Seq("wbanIden"))

    val tempLocDs = tempDs.join(stationDs, tempDs("wbanIden") === stationDs("wbanIden") &&
      tempDs("stnIden") === stationDs("stnIden")
    ).drop("stnIden").drop("wbanIden").as[RawMeasurement]

    tempLocDs.map { r =>
      SparkMeasurement(Date.valueOf(LocalDate.of(r.year, r.month, r.day)), r.lat, r.lon, r.temp)
    }
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    readMeasurements(year, stationsFile, temperaturesFile).rdd.
      map(r => (r.date.toLocalDate, Location(r.lat, r.lon), r.temp)).collect().toSeq
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    // Tested the performance of entire operation in RDD versus Dataframe
    // The performance results were:
    //    - RDD time taken:  9104.672457 ms
    //    - DF time taken:   7823.963467 ms

    //    val executionTimeRDD = withWarmer(new Warmer.Default) measure {
    //      val rdd = spark.sparkContext.parallelize(records.toSeq)
    //      val latLonAvg = rdd
    //        .map(r => (r._2, r._3))
    //        .aggregateByKey((0.0d, 0.0d))(
    //          (acc, value) => (acc._1 + value, acc._2 + 1f),
    //          (acc1, acc2) => (acc1._1 + acc2._1, acc1._2 + acc2._2))
    //        .mapValues(sumCount => sumCount._1 / sumCount._2)
    //        .collect().toSeq
    //    }

    val rdd = spark.sparkContext.parallelize(records.map { r => (r._2.lat, r._2.lon, r._3.toDouble) }.toSeq)
    val latLonAvg = rdd.toDF("lat", "lon", "temp").groupBy('lat, 'lon)
      .agg(avg('temp)).as("temp")

    latLonAvg.rdd.map(r => (Location(r.getDouble(0), r.getDouble(1)), r.getDouble(2))).collect().toSeq
  }

}
