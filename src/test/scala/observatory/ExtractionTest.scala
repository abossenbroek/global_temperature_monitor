package observatory

import org.scalatest.{BeforeAndAfterAll, FunSuite}

trait ExtractionTest extends FunSuite with BeforeAndAfterAll {
  import org.apache.log4j.{Level, Logger}

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  def initializeExtraction(): Boolean =
    try {
      Extraction
      true
    } catch {
      case ex: Throwable =>
        println(ex.getMessage)
        ex.printStackTrace()
        false
    }

  override def afterAll(): Unit = {
    assert(initializeExtraction(), "cannot start spark")
    import Extraction._
    spark.stop()
  }

  test("temperature extraction works") {
    assert(initializeExtraction(), "cannot start spark")
    import Extraction._
    val temps = locateTemperatures(2000, "/stations.csv", "/temperature_test.csv")
    assert(temps.size === 9, "Expected nine results")
  }
}