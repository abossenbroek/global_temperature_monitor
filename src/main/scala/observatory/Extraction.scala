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

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Global temperature loader")
      .config("spark.master", "local")
      .config("spark.driver.memory", "1600M")
      .config("spark.executor.memory", "1600M")
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
                                  temp: Temperature
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
  def loadStationData(stationsFile: String): DataFrame = {
    //import org.apache.spark.sql.expressions.scalalang.typed._
    import org.apache.spark.sql.functions._

    val stationData = readCsv[StationData](stationsFile)
    val cleanedStationData = stationData.filter($"lat".isNotNull && $"lon".isNotNull)

    cleanedStationData.toDF("stnIden", "wbanIden", "lat", "lon")
      .na.fill(0, Seq("stnIden", "wbanIden"))
      .withColumn("joinKeyStation", 'stnIden * 10000 + 'wbanIden)
      .groupBy('joinKeyStation)
      .agg(
        avg('lat).as("lat"),
        avg('lon).as("lon"))
  }

  /**
    * Load the temperature data for a certain year. Data is aggregated by station identifier, year, month, date to
    * ensure daily averages are calculated.
    *
    * @param year             The year for which data should be selected
    * @param temperaturesFile The csv files that should be loaded for temperature measurements
    * @return A dataset of temperature measurements where temperatures are measured in Celsius
    */
  def loadTemperatures(temperaturesFile: String): DataFrame = {
    import org.apache.spark.sql.functions._

    def f2c(f: Double): Double = (f - 32f) * 5f / 9f

    val temperatureData = readCsv[TemperatureData](temperaturesFile)
      .as[TemperatureYearData]

    temperatureData.toDF()
      .na.fill(0, Seq("stnIden", "wbanIden"))
      .withColumn("joinKeyWeather", 'stnIden * 10000 + 'wbanIden)
      .groupBy('joinKeyWeather, 'month, 'day)
      .agg(
        avg('temp).as("temp")
      )
      .withColumn("temp_celsius", ('temp - 32f) * 5f / 9f)
      .drop('temp)
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
  def readMeasurements(stationsFile: String, temperaturesFile: String): DataFrame = {
    val stationDs = loadStationData(stationsFile)
    val tempDs = loadTemperatures(temperaturesFile)

    tempDs
      .join(stationDs, 'joinKeyStation === 'joinKeyWeather)
      .drop("joinKeyStation", "joinKeyWeather")
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    readMeasurements(stationsFile, temperaturesFile).rdd.map(row => {
      (LocalDate.of(year, row.getInt(0), row.getInt(1)), Location(row.getDouble(3), row.getDouble(4)), row.getDouble(2))
    }).collect.toSeq
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records
      .par
      .groupBy(_._2)
      .mapValues(
        locationData => locationData.foldLeft(0d)(
          (sum, row) => sum + row._3
        ) / locationData.size
      ).seq
  }

}
