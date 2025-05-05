package reps

//Team members name: Mengshi Qi 000832579
//                   Zhexi Dong 000824286



import akka.actor.ActorSystem
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, blocking}
import scala.concurrent.ExecutionContext.Implicits.global
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale
import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}
import scala.io.StdIn
import reps.control.RenewableControl.RenewablePlant._
import reps.datacollection.fetchGeneratedEnergyData.fetchEnergyData
import reps.views.ViewPlantData.choice
import reps.dataanalysis.analysisGeneratedEnergyData.analyzeData
import reps.control.RenewableControl._
import reps.alerts.generateAlert.generateAlerts

sealed trait MenuOption
object MenuOption {
  case object ViewData extends MenuOption
  case object AnalyzeData extends MenuOption
  case object Alerts extends MenuOption
  case object Control extends MenuOption
  case object Exit extends MenuOption

  def fromInt(input: Int): Option[MenuOption] = input match {
    case 1 => Some(ViewData)
    case 2 => Some(AnalyzeData)
    case 3 => Some(Alerts)
    case 4 => Some(Control)
    case 5 => Some(Exit)
    case _ => None
  }

  val menuItems: Seq[(Int, String)] = Seq(
    1 -> "View Power Plant Data",
    2 -> "Analyze Energy Generation Data",
    3 -> "Generate Alerts",
    4 -> "Control Renewable Plants",
    5 -> "Exit"
  )
}

object Main {
  import MenuOption._

  private val formatter = new DecimalFormat("#.##", new DecimalFormatSymbols(Locale.US))
  private val statisticsLabels = Array("Mean", "Median", "Mode", "Range", "Midrange")
  private val datasetLabels = Array("Solar", "Wind", "Hydro")
  private val dataSources = Seq(
    "https://data.fingrid.fi/api/datasets/248/data" -> "solar.csv",
    "https://data.fingrid.fi/api/datasets/191/data" -> "hydro.csv",
    "https://data.fingrid.fi/api/datasets/75/data" -> "wind.csv"
  )

  private def displayMenu(): Unit = {
    println("\n--- Renewable Energy Management ---")
    menuItems.foreach { case (num, label) => println(s"$num. $label") }
  }

  private def safeReadInt(prompt: String): Int = {
    print(prompt)
    Try(StdIn.readInt()).getOrElse {
      println("Invalid input. Please enter a number.")
      safeReadInt(prompt)
    }
  }
// Error handling for invalid input
  @tailrec
  private def getUserChoice: MenuOption = {
    MenuOption.fromInt(safeReadInt("Your choice: ")) match {
      case Some(option) => option
      case None =>
        println("Invalid option. Try again.")
        getUserChoice
    }
  }

  private def handleOption(option: MenuOption): Unit = option match {
    case ViewData       => choice()
    case AnalyzeData    => showAnalysis()
    case Alerts         => generateAlerts()
    case Control        => runRenewableControlApp()
    case Exit           => println("Exiting...")
  }

  private def showAnalysis(): Unit = {
    println()
    val results = analyzeData("data/solar.csv", "data/wind.csv", "data/hydro.csv").flatten
    results.zip(datasetLabels).foreach { case (stats, label) =>
      println(s"$label Data:")
      stats.zip(statisticsLabels).foreach { case (value, name) =>
        println(s"$name: ${formatter.format(value)}")
      }
      println()
    }
  }

  private def fetchInitialData(): Unit = {
    val fetchTask = Future {
      dataSources.foreach { case (url, file) =>
        fetchEnergyData(url, file)
        println(s"Fetched $file. Waiting to avoid rate limit...")
        blocking(Thread.sleep(5000))
      }
    }
    Await.result(fetchTask, Duration.Inf)
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("RenewableEnergySystem")

    fetchInitialData()

    system.scheduler.scheduleAtFixedRate(15.minutes, 15.minutes) { () =>
      dataSources.foreach { case (url, file) => fetchEnergyData(url, file) }
    }

    Iterator
      .continually {
        displayMenu()
        getUserChoice
      }
      .takeWhile(_ != Exit)
      .foreach(handleOption)

    system.terminate()
  }
}
