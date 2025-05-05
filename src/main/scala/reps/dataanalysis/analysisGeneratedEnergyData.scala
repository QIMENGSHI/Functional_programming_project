package reps.dataanalysis

//Team members name: Mengshi Qi 000832579
//                   Zhexi Dong 000824286




import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.math.floor
import scala.util.control.NonFatal

object analysisGeneratedEnergyData {

  // Reads a CSV file and splits lines by delimiter
  private def readDataCsv(fileName: String)(delimiter: String): Option[List[Array[String]]] = {
    try {
      val bufferedSource = Source.fromFile(fileName)
      try Some(bufferedSource.getLines().toList.map(_.split(delimiter).map(_.trim)))
      finally bufferedSource.close()
    } catch {
      case NonFatal(_) =>
        println(s"Error: Unable to read from $fileName.")
        None
    }
  }

  // Extracts numerical values from the CSV, skipping header
  private def extractValues(data: List[Array[String]], headerIndex: Int): Option[Array[Double]] = {
    val header = data(headerIndex)
    val valueCol = header.indexOf("value")

    if (valueCol == -1) {
      println("Error: 'value' column not found.")
      None
    } else {
      try {
        Some(data.drop(headerIndex + 1).map(_(valueCol).toDouble).toArray)
      } catch {
        case _: NumberFormatException =>
          println("Error: One or more values could not be parsed as Double.")
          None
      }
    }
  }

  // Sorts an array in-place using insertion sort
  private def insertionSort(data: Array[Double]): Option[Array[Double]] = {
    try {
      for (i <- 1 until data.length) {
        var j = i
        while (j > 0 && data(j - 1) > data(j)) {
          val tmp = data(j)
          data(j) = data(j - 1)
          data(j - 1) = tmp
          j -= 1
        }
      }
      Some(data)
    } catch {
      case NonFatal(_) => None
    }
  }

  // Computes basic statistics from data
  private def calculateStatistics(data: Option[Array[Double]]): Option[Array[Double]] = {
    data.flatMap { raw =>
      insertionSort(raw).flatMap { sorted =>
        for {
          meanVal     <- mean(sorted)
          medianVal   <- median(sorted)
          modeVal     <- mode(sorted)
          rangeVal    <- range(sorted)
          midrangeVal <- midrange(sorted)
        } yield Array(meanVal, medianVal, modeVal, rangeVal, midrangeVal)
      }
    }
  }

  private def mean[T: Numeric](data: Array[T]): Option[Double] = {
    if (data.isEmpty) None
    else {
      val num = implicitly[Numeric[T]]
      Some(num.toDouble(data.sum) / data.length)
    }
  }

  private def median(data: Array[Double]): Option[Double] =
    if (data.isEmpty) None
    else Some(data(floor(data.length / 2.0).toInt))

  private def mode(data: Array[Double]): Option[Double] = {
    if (data.isEmpty) None
    else {
      val counts = mutable.Map[Double, Int]()
      data.foreach(d => counts.updateWith(d) {
        case Some(count) => Some(count + 1)
        case None        => Some(1)
      })
      counts.maxByOption(_._2).map(_._1)
    }
  }

  private def range(data: Array[Double]): Option[Double] =
    if (data.isEmpty) None else Some(data.last - data.head)

  private def midrange(data: Array[Double]): Option[Double] =
    if (data.isEmpty) None else Some((data.head + data.last) / 2)

  // Main analysis method for solar, wind, and hydro files
  def analyzeData(solarPath: String, windPath: String, hydroPath: String): List[Option[Array[Double]]] = {
    val readCsv = readDataCsv(_: String)(",")
    List(solarPath, windPath, hydroPath).map { path =>
      for {
        csv      <- readCsv(path)
        values   <- extractValues(csv, 0)
        analysis <- calculateStatistics(Some(values))
      } yield analysis
    }
  }
}
