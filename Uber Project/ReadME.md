Uber Rides 2014 Shiny App

This Shiny application loads six months (Apr–Sep 2014) of Uber ride data, processes date and location information, and presents multiple interactive visualizations, including bar charts, heatmaps, and a geospatial map.

File Structure

```
Uber Project/
├── Data			 # Storage for data
├── app.R           # Main Shiny application  
└── README.md       # Project documentation
```

Prerequisites


Packages: shiny, ggplot2, dplyr, DT, rsconnect, readxl, RCurl, lubridate, janitor, purrr, leaflet, scales, bslib


Data Loading

Download and bind CSVs directly from GitHub raw URLs:
```
urls <- c(
  apr  = "https://raw.githubusercontent.com/TommyAnderson/Data-332/main/Uber%20Project/Data/uber-raw-data-apr14.csv",
  may  = "https://raw.githubusercontent.com/TommyAnderson/Data-332/main/Uber%20Project/Data/uber-raw-data-may14.csv",
  jun  = "https://raw.githubusercontent.com/TommyAnderson/Data-332/main/Uber%20Project/Data/uber-raw-data-jun14.csv",
  jul  = "https://raw.githubusercontent.com/TommyAnderson/Data-332/main/Uber%20Project/Data/uber-raw-data-jul14.csv",
  aug  = "https://raw.githubusercontent.com/TommyAnderson/Data-332/main/Uber%20Project/Data/uber-raw-data-aug14.csv",
  sep  = "https://raw.githubusercontent.com/TommyAnderson/Data-332/main/Uber%20Project/Data/uber-raw-data-sep14.csv"
)
data_list <- lapply(urls, read.csv, stringsAsFactors = FALSE)
uber_data <- bind_rows(data_list, .id = "month")
```
Data Cleaning

Clean column names and extract date/time features:
```
uber_data <- uber_data %>%
  clean_names() %>%
  mutate(
    date_time = mdy_hms(date_time),   # parse to POSIXct
    date      = as_date(date_time),   # date only
    month     = month(date_time, label = TRUE, abbr = FALSE),
    day       = day(date_time),
    hour      = hour(date_time),
    wday      = wday(date_time, label = TRUE),
    week      = week(date_time)
  )
```
Summary Tables

Precompute counts for each visualization:
```
by_month      <- uber_data %>% count(month)
by_hour       <- uber_data %>% count(hour)
by_hour_month <- uber_data %>% count(hour, month)
# ... and similar for day, weekday, base, heatmaps
```
UI Layout

Uses a Bootstrap 5 theme via bslib and a collapsible navbar:
```
ui <- navbarPage(
  title = "Uber Rides 2014",
  theme = bs_theme(bootswatch = "flatly", primary = "#2C3E50"),
  collapsible = TRUE,
  tabPanel("Overview", ...),
  tabPanel("Hourly",   ...),
  tabPanel("Daily",    ...),
  tabPanel("Base × Month", ...),
  tabPanel("Heatmaps", ...),
  tabPanel("Map", leafletOutput("map"))
)
```
Server Logic

Render plots and tables; e.g., monthly summary:
```
output$plot_month <- renderPlot({
  ggplot(by_month, aes(month, n)) +
    geom_col(fill = "steelblue") +
    labs(x = "Month", y = "Trips") +
    theme_minimal()
})
```
Geospatial Map

Clustered circle markers for performance:
```
output$map <- renderLeaflet({
  leaflet(uber_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      lng            = ~lon,
      lat            = ~lat,
      radius         = 4,
      stroke         = FALSE,
      fillOpacity    = 0.6,
      clusterOptions = markerClusterOptions(),
      popup          = ~paste0(
                         "<b>Base:</b> ", base, "<br>",
                         "<b>When:</b> ", format(date_time, "%Y-%m-%d %H:%M")
                       )
    )
})
```
Deployment

The app is deployed at: https://tommyanderson.shinyapps.io/AndersonUber/


