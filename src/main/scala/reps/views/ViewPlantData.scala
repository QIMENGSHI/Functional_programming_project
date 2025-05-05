package reps.views

//Team members name: Mengshi Qi 000832579
//                   Zhexi Dong 000824286





import com.github.tototoshi.csv._
import java.io.File
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.util.{Try, Success, Failure}
import scala.io.StdIn

object ViewPlantData {
  private val csvDateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSX")
  private val displayDateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd h:mm a")

  private def sortByTimestamp(data: List[List[String]]): Option[List[List[String]]] = Try {
    val header :: tail = data
    header :: tail.sortBy(row => LocalDateTime.parse(row.head, csvDateFormat))
  }.toOption

  private def sortByValue(data: List[List[String]]): Option[List[List[String]]] = Try {
    val header :: tail = data
    header :: tail.sortBy(row => row.last.toDouble)
  }.toOption

  private def filterByTimePeriod(data: List[List[String]], amount: Long, unit: ChronoUnit): Option[List[List[String]]] = {
    val now = LocalDateTime.now(ZoneId.of("UTC"))
    val start = now.minus(amount, unit)
    Try {
      val header :: tail = data
      val filtered = header :: tail.filter { row =>
        Try(LocalDateTime.parse(row.head, csvDateFormat)).toOption match {
          case Some(ts) => ts.isAfter(start) && ts.isBefore(now)
          case None =>
            println(s"Warning: Could not parse timestamp: ${row.head}")
            false
        }
      }
      Some(filtered)
    }.getOrElse(None)
  }
  // This function searches for a specific date in the data
  private def searchByDate(data: List[List[String]], input: String): Option[List[List[String]]] = {
    Try {
      val date = LocalDate.parse(input, DateTimeFormatter.ofPattern("dd/MM/yyyy"))
      val header :: tail = data
      val filtered = header :: tail.filter { row =>
        Try(LocalDateTime.parse(row.head, csvDateFormat).toLocalDate.isEqual(date)).getOrElse {
          println(s"Failed to parse date in row: ${row.head}")
          false
        }
      }
      Some(filtered)
    }.getOrElse {
      println(s"Failed to parse input date: $input")
      None
    }
  }

  private def formatTimestamps(data: List[List[String]]): List[List[String]] = {
    val header :: tail = data
    val updatedHeader = header.updated(0, "").updated(1, "")
    val formattedRows = tail.map { row =>
      Try {
        row.updated(0, LocalDateTime.parse(row(0), csvDateFormat).format(displayDateFormat))
           .updated(1, LocalDateTime.parse(row(1), csvDateFormat).format(displayDateFormat))
      }.getOrElse(row)
    }
    updatedHeader :: formattedRows
  }

  private def displayData(fileName: String, sortBy: Option[String]): Unit = {
    val data = Try(CSVReader.open(new File(fileName)).all()).toOption
    data.flatMap { raw =>
      val sorted = sortBy match {
        case Some("timestamp") => sortByTimestamp(raw)
        case Some("value")     => sortByValue(raw)
        case Some("search") =>
          print("Enter the date to search (dd/MM/yyyy): ")
          searchByDate(raw, StdIn.readLine())
        case Some("filter") =>
          println("1. Last 24 hours\n2. Last 7 days\n3. Last 30 days")
          print("Enter your filter choice: ")
          StdIn.readInt() match {
            case 1 => filterByTimePeriod(raw, 24, ChronoUnit.HOURS)
            case 2 => filterByTimePeriod(raw, 7, ChronoUnit.DAYS)
            case 3 => filterByTimePeriod(raw, 1, ChronoUnit.MONTHS)
            case _ =>
              println("Invalid period.")
              None
          }
        case _ => Some(raw)
      }
      sorted
    }.foreach { result =>
      println("Formatted Results:")
      formatTimestamps(result).foreach(row => println(row.mkString("\t")))
    }
  }

  private def getSortOption(): Option[String] = {
    println("1. Sort by timestamp\n2. Sort by value\n3. Search by date\n4. Filter by time period")
    print("Enter your sorting choice: ")
    // StdIn.readInt() match {
    //   case 1 => Some("timestamp")
    //   case 2 => Some("value")
    //   case 3 => Some("search")
    //   case 4 => Some("filter")
    //   case _ =>
    //     println("Invalid input. Please enter a number between 1 and 4.")
    //     None
    // }
     scala.util.Try(StdIn.readLine().trim.toInt).toOption match {
      case Some(1) => Some("timestamp")
      case Some(2) => Some("value")
      case Some(3) => Some("search")
      case Some(4) => Some("filter")
      case _ =>
        println("Invalid input. Please enter a number between 1 and 4.")
        None
    }
  }

  def choice(): Unit = {
    println("Which data do you want to view?")
    println("1. Solar\n2. Wind\n3. Hydro\n4. All")
    print("Enter your choice: ")
    val plantOption = StdIn.readInt()

    val plantMap = Map(
      1 -> ("Solar", "data/solar.csv"),
      2 -> ("Wind", "data/wind.csv"),
      3 -> ("Hydro", "data/hydro.csv")
    )

    plantOption match {
      case 1 | 2 | 3 =>
        val (name, file) = plantMap(plantOption)
        getSortOption().foreach(opt => displayPlantData(name, file, Some(opt)))
      case 4 =>
        plantMap.foreach { case (i, (name, file)) =>
          displayPlantData(name, file, Some("timestamp"))
        }
      case _ => println("Invalid choice")
    }
  }

  private def displayPlantData(name: String, file: String, sortBy: Option[String]): Unit = {
    println(s"\n$name Data:")
    displayData(file, sortBy)
  }
}
