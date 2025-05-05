package reps.alerts

//Team members name: Mengshi Qi 000832579
//                   Zhexi Dong 000824286




import reps.dataanalysis.analysisGeneratedEnergyData.analyzeData
import reps.control.RenewableControl.readPlantStatusFromFile

object generateAlert {

  // Threshold values for average generation
  private val solarThreshold = 200.0
  private val windThreshold = 3000000.0 // For demonstration purposes
  private val hydroThreshold = 1000.0

  def generateAlerts(): Unit = {
    val results = analyzeData("data/solar.csv", "data/wind.csv", "data/hydro.csv").flatten
    // Check if results contain all three datasets
    if (results.size != 3) {
      println("Error: Insufficient data to generate alerts.")
      return
    }

    val List(solarStats, windStats, hydroStats) = results
    // Check if the datasets are empty
    // if (solarStats.isEmpty || windStats.isEmpty || hydroStats.isEmpty) {
    //   println("Error: One or more datasets are empty.")
    //   return
    // }

    checkThresholdAlert("Solar", solarStats(0), solarThreshold)
    checkThresholdAlert("Wind", windStats(0), windThreshold)
    checkThresholdAlert("Hydro", hydroStats(0), hydroThreshold)

    // Check if the plants are running
    readPlantStatusFromFile() match {
      case Some((solarRunning, windRunning, hydroRunning)) =>
        println(s"Solar running: $solarRunning, Wind running: $windRunning, Hydro running: $hydroRunning")
        if (!solarRunning) println("Alert: Solar plant is not running.")
        if (!windRunning) println("Alert: Wind plant is not running.")
        if (!hydroRunning) println("Alert: Hydro plant is not running.")
      case None =>
        println("Error: Unable to read plant status from file.")
    }
  }
  // Check if the average generation is below the threshold
  // and print an alert message
  private def checkThresholdAlert(plant: String, value: Double, threshold: Double): Unit = {
    if (value < threshold) {
      println(s"Alert: Average $plant energy generation is below $threshold.")
    }
  }
}
