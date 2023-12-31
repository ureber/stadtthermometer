# Load required libraries
library(shiny)
library(tidyverse)

# Load and prepare data

# List all CSV files in the folder
csv_files <- list.files(path = "data/", pattern = ".csv")
# Initialize an empty list to store data frames
data_frames_list <- list()
# Loop through the CSV files and read them, then append to the list
for (file in csv_files) {
  data <- read.csv(paste0("data/", file))
  data_frames_list <- c(data_frames_list, list(data))
}
# Row bind all the data frames in the list
data <- do.call(rbind, data_frames_list) %>%
  mutate(DateTime = as.POSIXct(epoch_utc, origin = "1970-01-01", tz = "UTC")) %>%
  rename(Temperature = temperatur, Station = sensorid, Humidity = relative_luftfeuchtigkeit) %>%
  arrange(DateTime)
# Get min and max temperature/humidity
measure_range <- data %>% 
  group_by(DateTime) %>% 
  summarize(min_temp = min(Temperature, na.rm = TRUE), max_temp = max(Temperature, na.rm = TRUE),
            min_hum = min(Humidity, na.rm = TRUE), max_hum = max(Humidity, na.rm = TRUE))
data <- left_join(data, measure_range, by = "DateTime")

# Define the UI for the Shiny app
ui <- fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  titlePanel("App zur explorativen Analyse der Daten aus dem Citizen-Science-Projekt «Stadt-Thermometer»"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Datumsbereich auswählen", start = min(data$DateTime), end = max(data$DateTime)),
      selectInput("station", "Erste Station: SensorID auswählen (Violett)", unique(data$Station)),
      selectInput("secondStation", "Zweite Station: SensorID auswählen (Grün)", c("None", unique(data$Station))),
      helpText(HTML("Die SensorID und weitere Informationen zu den Stationen können auf dem <a href='https://stadtplan.winterthur.ch/?topic=Stadtthermometer_Juli23' target='_blank'>Stadtplan der Stadt Winterthur</a> nachgeschaut werden. Weitere Informationen zum Projekt und die hier angezeigten Rohdaten gibt es auf der offiziellen <a href='https://stadt.winterthur.ch/themen/leben-in-winterthur/energie-umwelt-natur/klimaanpassung/stadt-thermometer' target='_blank'>Projekt-Website</a>.")),
      helpText("Der graue Bereich zeigt die Bandbreite der gemessenen Werte aller zu diesem Zeitpunkt aktiven Stationen."),
      helpText(HTML("App von <a href='https://uelireber.ch' target='_blank'>Ueli Reber</a>, Code auf <a href='https://github.com/ureber/stadtthermometer' target='_blank'>GitHub</a> (CC BY-SA), V 2023-11-10"), style = "font-size: 9px;"),
      hr(),
      h4("Zusammenfassende Statistiken"),
      tableOutput("summaryTable")
    ),
    mainPanel(
      plotOutput("temperaturePlot"),
      plotOutput("humidityPlot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Filter data based on user inputs
  filteredData <- reactive({
    data %>%
      filter(DateTime >= input$dateRange[1] & DateTime <= input$dateRange[2])
  })
  
  # Create the temperature plot
  output$temperaturePlot <- renderPlot({
    gg <- ggplot(filteredData(), aes(x = DateTime, y = Temperature, color = Station)) +
      geom_ribbon(data = filteredData(), aes(ymin = min_temp, ymax = max_temp),
                  fill = "gray80", color = NA) +
      labs(title = "Temperatur", x = "Zeit", y = "Temperatur (°C)") +
      scale_color_discrete(name = "Station") +
      theme_minimal()
    
    # Highlight the selected first station's data in red
    if (!is.null(input$station)) {
      gg <- gg + geom_line(data = filteredData() %>% filter(Station == input$station), color = "#af8dc3", linewidth = 1.5)
    }
    
    # Highlight the selected second station's data in blue (if not "None" is selected)
    if (input$secondStation != "None") {
      gg <- gg + geom_line(data = filteredData() %>% filter(Station == input$secondStation), color = "#7fbf7b", linewidth = 1.5)
    }
    
    print(gg)
  })
  
  # Create the humidity plot
  output$humidityPlot <- renderPlot({
      gg <- ggplot(filteredData(), aes(x = DateTime, y = Humidity, color = Station)) +
        geom_ribbon(data = filteredData(), aes(ymin = min_hum, ymax = max_hum),
                    fill = "gray80", color = NA) +
        labs(title = "Relative Luftfeuchtigkeit", x = "Zeit", y = "Relative Luftfeuchtigkeit (%)") +
        scale_color_discrete(name = "Station") +
        theme_minimal()
      
      # Highlight the selected first station's data in red
      if (!is.null(input$station)) {
        gg <- gg + geom_line(data = filteredData() %>% filter(Station == input$station), color = "#af8dc3", linewidth = 1.5)
      }
      
      # Highlight the selected second station's data in blue (if not "None" is selected)
      if (input$secondStation != "None") {
        gg <- gg + geom_line(data = filteredData() %>% filter(Station == input$secondStation), color = "#7fbf7b", linewidth = 1.5)
      }
      
      print(gg)
  }) 
  
  # Display summary information as a table with rounded numbers
  output$summaryTable <- renderTable({
    
    # Summary data for all stations
    summary_all_stations <- data.frame(
      Metric = rep(c("Mittlere Temperatur", "Tiefste Temperatur", "Höchste Temperature", "Mittlere rel. Luftfeuchtigkeit", "Tiefste rel. Luftfeuchtigkeit", "Höchste rel. Luftfeuchtigkeit"), each = 1),
      Value = c(
        mean(filteredData()$Temperature, na.rm = TRUE),
        min(filteredData()$Temperature, na.rm = TRUE),
        max(filteredData()$Temperature, na.rm = TRUE),
        mean(filteredData()$Humidity, na.rm = TRUE),
        min(filteredData()$Humidity, na.rm = TRUE),
        max(filteredData()$Humidity, na.rm = TRUE)
      )
    )
    
    # Summary data for Station_1
    summary_station_1 <- data.frame(
      Metric = rep(c("Mittlere Temperatur", "Tiefste Temperatur", "Höchste Temperature", "Mittlere rel. Luftfeuchtigkeit", "Tiefste rel. Luftfeuchtigkeit", "Höchste rel. Luftfeuchtigkeit"), each = 1),
      Value = c(
        mean(filteredData()$Temperature[filteredData()$Station == input$station], na.rm = TRUE),
        min(filteredData()$Temperature[filteredData()$Station == input$station], na.rm = TRUE),
        max(filteredData()$Temperature[filteredData()$Station == input$station], na.rm = TRUE),
        mean(filteredData()$Humidity[filteredData()$Station == input$station], na.rm = TRUE),
        min(filteredData()$Humidity[filteredData()$Station == input$station], na.rm = TRUE),
        max(filteredData()$Humidity[filteredData()$Station == input$station], na.rm = TRUE)
      )
    )
    
    # Summary data for Station_2 (if selected)
    if (input$secondStation != "None") {
      summary_station_2 <- data.frame(
        Metric = rep(c("Mittlere Temperatur", "Tiefste Temperatur", "Höchste Temperature", "Mittlere rel. Luftfeuchtigkeit", "Tiefste rel. Luftfeuchtigkeit", "Höchste rel. Luftfeuchtigkeit"), each = 1),
        Value = c(
          mean(filteredData()$Temperature[filteredData()$Station == input$secondStation], na.rm = TRUE),
          min(filteredData()$Temperature[filteredData()$Station == input$secondStation], na.rm = TRUE),
          max(filteredData()$Temperature[filteredData()$Station == input$secondStation], na.rm = TRUE),
          mean(filteredData()$Humidity[filteredData()$Station == input$secondStation], na.rm = TRUE),
          min(filteredData()$Humidity[filteredData()$Station == input$secondStation], na.rm = TRUE),
          max(filteredData()$Humidity[filteredData()$Station == input$secondStation], na.rm = TRUE)
        )
      )
      
      # Combine data frames for all stations, Station_1, and Station_2
      summary_data <- left_join(summary_all_stations, summary_station_1, by = "Metric") %>%
        rename(Alle_Stationen = Value.x, Station_1 = Value.y) %>%
        left_join(summary_station_2, by = "Metric") %>%
        rename(Kennzahl = Metric, Station_2 = Value)
    } else {
      # Combine data frames for all stations and Station_1
      summary_data <- left_join(summary_all_stations, summary_station_1, by = "Metric") %>%
        rename(Kennzahl = Metric, Alle_Stationen = Value.x, Station_1 = Value.y)
    }
    
    return(summary_data)
  }, row.names = FALSE)
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
