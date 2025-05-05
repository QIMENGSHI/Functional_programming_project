package reps.datacollection

//Team members name: Mengshi Qi 000832579
//                   Zhexi Dong 000824286




import java.io.{BufferedReader, File, FileWriter, InputStreamReader}
import java.net.{HttpURLConnection, URL}
import scala.io.Source
import org.json4s._
import org.json4s.native.JsonMethods._
import scala.language.postfixOps

object fetchGeneratedEnergyData {
  
  implicit val formats: DefaultFormats.type = DefaultFormats

  // Reads the API key from a .env file
  private def getApiKey: Option[String] = {
    try {
      val source = Source.fromFile("src/main/scala/reps/.env")
      try Some(source.mkString.trim)
      finally source.close()
    } catch {
      case _: Exception =>
        println("Error: Unable to read API key from .env file.")
        None
    }
  }

  // Main entry: fetches energy data and writes to CSV
  def fetchEnergyData(apiUrl: String, fileName: String): Option[Unit] = {
    for {
      apiKey <- getApiKey
      response <- getApiResponse(apiUrl, apiKey)
      _ <- writeToCsv(response, fileName)
    } yield ()
  }

  // Makes a GET request to the API
  private def getApiResponse(urlStr: String, apiKey: String): Option[String] = {
    // Check if the API key is valid
    try {
      val url = new URL(urlStr)
      val conn = url.openConnection().asInstanceOf[HttpURLConnection]
      conn.setRequestMethod("GET")
      conn.setRequestProperty("Cache-Control", "no-cache")
      conn.setRequestProperty("x-api-key", apiKey)

      val code = conn.getResponseCode
      // Check if the response code is OK
      if (code == HttpURLConnection.HTTP_OK) {
        // Read the response
        val reader = new BufferedReader(new InputStreamReader(conn.getInputStream))
        val response = Iterator.continually(reader.readLine()).takeWhile(_ != null).mkString
        reader.close()
        Some(response)
      } else {
        println(s"Failed to retrieve data. HTTP response code: $code")
        None
      }
    } catch {
      case _: Exception =>
        println("Error during API request.")
        None
    }
  }

  // Parses and appends non-duplicate data to CSV
  private def writeToCsv(jsonString: String, fileName: String): Option[Unit] = {
    try {
      // Parse the JSON response
      // and extract the relevant data
      // Check if the file exists
      val filePath = s"data/$fileName"
      val file = new File(filePath)
      val exists = file.exists()

      val json = parse(jsonString)
      val entries = (json \ "data").extract[List[JValue]]
      // Check if the file exists and read existing lines
      // to avoid duplicates
      val existingLines = if (exists) {
        val source = Source.fromFile(filePath)
        try source.getLines().toSet finally source.close()
      } else Set.empty[String]

      val writer = new FileWriter(filePath, true)
      if (!exists) writer.write("startTime,endTime,value\n")
      // Extract and write non-duplicate entries
      entries.foreach { item =>
        val start = (item \ "startTime").extractOpt[String]
        val end = (item \ "endTime").extractOpt[String]
        val value = (item \ "value").extractOpt[Double]

        for {
          s <- start
          e <- end
          v <- value
        } {
          val line = s"$s,$e,$v"
          if (!existingLines.contains(line)) {
            writer.write(s"$line\n")
          }
        }
      }

      writer.close()
      // If the file didn't exist, print a success message
      // to indicate that it was created
      // and the data was written
      if (!exists) println("CSV file created successfully.")
      Some(())
    } catch {
      case _: Exception =>
        // Handle any exceptions that occur during file writing
        println("Error processing response or writing to file.")
        None
    }
  }
}
