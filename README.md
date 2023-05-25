



<h3 align="center"></h3>  Haskell Web Data Harvesting App </h3>

<div align="center">

  [![code coverage](coverage.svg "Code coverage")]()
</div>

---


## 🧐 About <a name = "about"></a>
The goal of this project is to develop a stack-based Haskell application for harvesting information from the web and saving it in a database. The application allows users to choose a website containing data of interest, download the data, parse it, and store it in a SQLite database. Users can also query the data and generate a JSON representation of the data. The application utilizes stack for project management and dependencies.


## Application Overview
The Haskell Web Data Harvesting App consists of several modules, each serving a specific purpose:

* Types.hs: This module defines the Haskell data types used in the application. It provides a structured representation for the downloaded data.

* Fetch.hs: In this module, a function is implemented to download the document (HTML, CSV, XML, or JSON) from the web. It handles HTTP requests and retrieves the data for further processing.

* Parse.hs: The Parse module is responsible for parsing the downloaded data into the defined Haskell data types. It ensures that the data is correctly extracted and converted into the appropriate format.

* Database.hs: This module is dedicated to interacting with the database. It creates the necessary tables, saves and retrieves data from the database, and handles database-related operations using the SQLite-simple library.

* Main.hs: The Main module serves as the entry point of the application. It provides an interactive user interface where users can create and initialize a SQLite database, download data from the web, save it to the database, run queries on the database, and perform other operations.

## Application Features
The Haskell Web Data Harvesting App offers the following features:

### Data Source and Extraction

The application allows users to choose a website containing the desired data. The data can be in HTML, CSV, XML, or JSON format. If the data is in HTML format, the application parses the HTML content to extract the relevant information. For structured data formats like CSV, XML, or JSON, the application directly retrieves the data and processes it.

### Database Interaction

The application uses SQLite as the database engine. The Database module creates the necessary tables to store the downloaded data. It provides functionality to save and retrieve data from the database using the defined Haskell data types. Users can run various queries on the database, such as selecting, joining, and updating data.

### JSON Representation

The Parse module includes a function that generates a JSON representation of the Haskell data and writes it to a file. This feature allows users to export the data in a JSON format for further analysis or integration with other systems.

### Documentation Generation

The application employs Haddock notation for code commenting. This allows the automatic generation of Haddock documentation for the app. Users can build the documentation using the command "stack build && cabal haddock --enable-documentation --open" and access the generated documentation through the provided file path.

## Extra Features
In addition to the specified requirements, the following extra features have been implemented:

### Local Database Dump in JSON Format

The application includes functionality to download the database tables in JSON format. Users can obtain a JSON dump of the database, which includes both station details and weather details.

### Database Table Update

The application enables users to update the average temperature column in the database. Users can input a station name, and the application updates the corresponding average temperature in the database.

### CSV File Insertion into Database

An additional feature has been implemented to insert data into the database from a CSV file. This allows users to directly import data from a CSV file into the database, expanding the options for data ingestion.

### Improved User Interface

Delays have been introduced in the main menu to enhance accessibility. This ensures that users have sufficient time to read the options and make their selections without feeling rushed.

### Error Handling

The application incorporates robust error handling mechanisms. It handles potential exceptions and anomalies during HTTP requests and user input. This prevents inaccurate data processing and ensures the application's stability and reliability.

## User Manual
To compile and run the application, execute the command "stack run" in the project directory.

To generate the Haddock documentation, use the following command:

```
stack build && cabal haddock --enable-documentation --open
The generated Haddock documentation can be accessed through the file path: haskell-project/haskell-project/dist-newstyle/build/aarch64-osx/ghc-9.4.3/haskell-project-0.1.0.0/doc/html/haskell-project/index.html
```

Upon running the application, the user is presented with a menu containing various options:

  * Download and Save Data: This option downloads the CSV file from the specified public URL, parses it using the Casava library, and inserts the data into the SQLite database. The data includes station details and weather details.

  * View Data by State: Users can input a state name, and the application retrieves and displays all the station and weather details for that particular state.

  * Get Random Stations: This option returns a list of ten random stations. These stations can be used as input for other options.

  * Top Stations by Temperature: Users can input "min" or "max" to retrieve the top ten stations with the minimum or maximum temperatures, respectively.

  * Update Average Temperature: Users can input a station name and a new average temperature. The application updates the average temperature column in the database for the specified station.

  * Download Database Dump: This feature allows users to download the database dump in JSON format. The JSON file contains the station details.

  * Download Weather Table Dump: Users can download a JSON dump of the weather table from the database.

The application also includes an API URL for retrieving the CSV file: "https://corgis-edu.github.io/corgis/datasets/csv/weather/weather.csv"

## Conclusion

The Haskell Web Data Harvesting App provides a powerful and versatile tool for harvesting web data and storing it in a database. It offers various features such as data extraction, database interaction, JSON representation, and documentation generation. The implementation of extra features, including local database dump, table update, CSV file insertion, improved user interface, and error handling, enhances the application's functionality and usability. By following the user manual, users can effectively compile, run, and utilize the application for their web data harvesting needs.





