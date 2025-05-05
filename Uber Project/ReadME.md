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

First the data was cleaned in a separate file to an rds file
```
uber_data <- csv_files %>%
  set_names(basename(.)) %>%                      
  map_dfr(read_csv, .id = "raw_file") %>%       
  clean_names() %>%                                
  rename(
    lat = any_of(c("lat","latitude")),
    lon = any_of(c("lon","longitude"))
  ) %>%
  mutate(
    date_time = mdy_hms(date_time),
    date      = as_date(date_time),
    month     = month(date_time, label = TRUE, abbr = FALSE),
    day       = day(date_time),
    hour      = hour(date_time),
    wday      = wday(date_time, label = TRUE),
    week      = week(date_time)
  )
```


Data Loading

Download and bind CSVs directly from GitHub raw URLs:
```
rds_path <- file.path("Data Clean", "uber_data_all.rds")

uber_data <- readRDS(rds_path)
```
Data Cleaning

Clean column names and extract date/time features, also in the separate cleaning file
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
    # sample a subset to reduce server load
    set.seed(42)
    sample_df <- uber_data %>% sample_n(min(100000, nrow(uber_data)))
    coords <- cbind(sample_df$lon, sample_df$lat)
    kde <- bkde2D(coords, bandwidth = c(0.005, 0.008), gridsize = c(150, 150))
    r <- raster(list(x = kde$x1, y = kde$x2, z = kde$fhat))
    r[r[] < 1] <- NA
    pal <- colorNumeric("Spectral", domain = r[], na.color = "transparent")
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addRasterImage(r, colors = pal, opacity = 0.6) %>%
      addLegend("bottomright", pal = pal, values = r[], title = "Density") %>%
      fitBounds(
        lng1 = min(sample_df$lon, na.rm = TRUE), lat1 = min(sample_df$lat, na.rm = TRUE),
        lng2 = max(sample_df$lon, na.rm = TRUE), lat2 = max(sample_df$lat, na.rm = TRUE)
      )
  })
}
```
Deployment

The app is deployed at: https://tommyanderson.shinyapps.io/AndersonUber/


