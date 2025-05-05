package reps.control

//Team members name: Mengshi Qi 000832579
//                   Zhexi Dong 000824286




import scala.io.Source
import scala.util.{Try, Success, Failure}
import java.io.{File, PrintWriter}

object RenewableControl {
  import Functor._

  trait RenewablePlant[A <: Plant] {
    def shutdown(plant: A): A
    def turnOn(plant: A): A
    def rotate(plant: A, degrees: Int): A
    def energyCapacity(plant: A): Double
  }

object Functor {
  // Define a Functor type class
  trait Functor[F[_]] {
    // The map function applies a function to the value inside the functor
    // and returns a new functor with the transformed value
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    // The map function for Option
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }
}


trait Plant {
  // Define the properties of a renewable plant
  def name: String
  def running: Boolean
}
// Define specific renewable plant types
  // Each plant type has its own properties and methods
  // For example, Wind, Solar, and Hydro plants
  case class Wind(name: String, running: Boolean = true, orientation: Int = 0) extends Plant
  case class Solar(name: String, running: Boolean = true, orientation: Int = 0) extends Plant
  case class Hydro(name: String, running: Boolean = true, orientation: Int = 0) extends Plant

  private val RotationAngle = 30

  private object Wind {
    // Define the operations for Wind plants
    implicit val windOps: RenewablePlant[Wind] = new RenewablePlant[Wind] {
      def shutdown(p: Wind) = { println("Shutting down wind plant..."); p.copy(running = false) }
      def turnOn(p: Wind) = { println("Turning on wind plant..."); p.copy(running = true) }
      def rotate(p: Wind, d: Int) = { println(s"Rotating wind plant by $d°"); p.copy(orientation = (p.orientation + d) % 360) }
      def energyCapacity(p: Wind) = 1e11
    }
  }

  private object Solar {
    // Define the operations for Solar plants
    implicit val solarOps: RenewablePlant[Solar] = new RenewablePlant[Solar] {
      def shutdown(p: Solar) = { println("Shutting down solar plant..."); p.copy(running = false) }
      def turnOn(p: Solar) = { println("Turning on solar plant..."); p.copy(running = true) }
      def rotate(p: Solar, d: Int) = { println(s"Rotating solar plant by $d°"); p.copy(orientation = (p.orientation + d) % 360) }
      def energyCapacity(p: Solar) = 5e10
    }
  }

  private object Hydro {
    // Define the operations for Hydro plants
    implicit val hydroOps: RenewablePlant[Hydro] = new RenewablePlant[Hydro] {
      // Hydro plants do not rotate, so we can define a no-op for rotation
      def shutdown(p: Hydro) = { println("Shutting down hydro plant..."); p.copy(running = false) }
      def turnOn(p: Hydro) = { println("Turning on hydro plant..."); p.copy(running = true) }
      def rotate(p: Hydro, d: Int) = { println("Hydro plants cannot rotate."); p }
      def energyCapacity(p: Hydro) = 2e11
    }
  }

  object RenewablePlant {
    // Interact with the plant
    def interactPlant[A <: Plant](plant: A)(implicit ops: RenewablePlant[A], functor: Functor[Option]): Option[A] = {
      println(s"Interacting with ${plant.name} (Running: ${plant.running})")
      
      val opsList = if (plant.running) List("Shutdown", "Rotate") else List("Turn On")
      
      opsList.zipWithIndex.foreach { case (op, i) => println(s"${i + 1}. $op") }
      print("Enter your choice: ")

      val input = scala.io.StdIn.readInt()
      input match {
        // Perform the operation based on user input
        case 1 if plant.running     => Some(ops.shutdown(plant))
        case 1 if !plant.running    => Some(ops.turnOn(plant))
        case 2 if plant.isInstanceOf[Wind] || plant.isInstanceOf[Solar] =>
          Some(ops.rotate(plant, RotationAngle))
        case _ =>
          println("Invalid operation.")
          Some(plant)
      }
    }
  }

  def readPlantStatusFromFile(): Option[(Boolean, Boolean, Boolean)] = Try {
    val lines = Source.fromFile("plant_status.txt").getLines().toList
    val wind  = lines.headOption.exists(_.split(":").last.trim == "true")
    val solar = lines.lift(1).exists(_.split(":").last.trim == "true")
    val hydro = lines.lift(2).exists(_.split(":").last.trim == "true")
    (wind, solar, hydro)
  }.toOption

  private def writePlantStatusToFile(wind: Boolean, solar: Boolean, hydro: Boolean): Unit = {
    val writer = new PrintWriter(new File("plant_status.txt"))
    writer.write(s"Wind Plant: $wind\nSolar Plant: $solar\nHydro Plant: $hydro\n")
    writer.close()
  }
  // Read the CSV file and calculate the sum of the third column
  private def calculateSumFromCSV(fileName: String): Double = {
    val source = Source.fromFile(fileName)
    val sum = source.getLines().drop(1).map(_.split(",")(2).toDouble).sum
    source.close()
    sum
  }
  // Check if the sum exceeds the capacity
  // If it does, print an alert and return true
  // Otherwise, return false
  private def checkOverload(plantName: String, sum: Double, capacity: Double): Boolean = {
    if (sum > capacity) {
      println(s"Alert: $plantName production exceeds capacity ($sum > $capacity). Shutting down.")
      true
    } else false
  }

  def runRenewableControlApp(): Unit = {
    // Read plant status from file or set default values
    val (windStatus, solarStatus, hydroStatus) = readPlantStatusFromFile().getOrElse((true, true, true))
    val wind = Wind("Wind Plant", windStatus)
    val solar = Solar("Solar Plant", solarStatus)
    val hydro = Hydro("Hydro Plant", hydroStatus)

    println("Select an option:")
    println("1. Wind Plant\n2. Solar Plant\n3. Hydro Plant\n4. Energy Sum vs Capacity\n5. Rotate Plant")
    print("Enter your choice: ")
    val input = scala.io.StdIn.readInt()
    // Perform the operation based on user input
    // Use Option to handle the case where no plant is selected
    val updatedPlantOpt = input match {
      case 1 => RenewablePlant.interactPlant(wind)
      case 2 => RenewablePlant.interactPlant(solar)
      case 3 => RenewablePlant.interactPlant(hydro)
      case 4 =>
        val windSum  = calculateSumFromCSV("data/wind.csv")
        val solarSum = calculateSumFromCSV("data/solar.csv")
        val hydroSum = calculateSumFromCSV("data/hydro.csv")

        val windCap  = Wind.windOps.energyCapacity(wind)
        val solarCap = Solar.solarOps.energyCapacity(solar)
        val hydroCap = Hydro.hydroOps.energyCapacity(hydro)

        println(f"Wind: $windSum%.2f / $windCap%.2f")
        println(f"Solar: $solarSum%.2f / $solarCap%.2f")
        println(f"Hydro: $hydroSum%.2f / $hydroCap%.2f")

        if (checkOverload("Wind", windSum, windCap)) writePlantStatusToFile(false, solar.running, hydro.running)
        else if (checkOverload("Solar", solarSum, solarCap)) writePlantStatusToFile(wind.running, false, hydro.running)
        else if (checkOverload("Hydro", hydroSum, hydroCap)) writePlantStatusToFile(wind.running, solar.running, false)
        None
      case 5 =>
        // Rotate the plant
        println("Choose a plant to rotate:\n1. Wind\n2. Solar")
        scala.io.StdIn.readInt() match {
          case 1 => Some(Wind.windOps.rotate(wind, RotationAngle))
          case 2 => Some(Solar.solarOps.rotate(solar, RotationAngle))
          case _ => None
        }
      case _ => None
    }

    updatedPlantOpt.foreach {
      // Update the plant status in the file
      case updated: Wind  => writePlantStatusToFile(updated.running, solar.running, hydro.running)
      case updated: Solar => writePlantStatusToFile(wind.running, updated.running, hydro.running)
      case updated: Hydro => writePlantStatusToFile(wind.running, solar.running, updated.running)
    }

    updatedPlantOpt.foreach { p =>
      println(s"Updated ${p.asInstanceOf[Plant].getClass.getSimpleName}: ${p.toString}")
    }
  }
}
