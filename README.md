You must register for a Fingrid API key at https://www.fingrid.fi/.
Save the key in a file named .env, placed at the same directory level as Main.scala. and then run this project
## Modules
- **alerts**: Generates alerts for low energy output and offline systems
- **control**: Control logic for turning on/off and rotating power plants
- **dataanalysis**: Calculates statistics like mean, median, mode, etc.
- **datacollection**: Pulls energy data from Fingrid APIs and stores it to CSV
- **views**: Displays sorted, filtered energy data to the user

## Running the Project
Make sure you have sbt installed.
```
sbt compile
sbt run
```
