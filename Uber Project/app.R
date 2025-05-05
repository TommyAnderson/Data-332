library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(readxl)
library(RCurl)
library(lubridate)
library(janitor)
library(purrr)
library(leaflet)
library(KernSmooth)
library(raster)      
library(scales)
library(bslib)

# Remove any existing objects
rm(list = ls())


rds_path <- file.path("Data Clean", "uber_data_all.rds")

uber_data <- readRDS(rds_path)


# Pre-compute summary tables
by_month      <- uber_data %>% count(month)
by_hour       <- uber_data %>% count(hour)
by_hour_month <- uber_data %>% count(hour, month)
by_day_month  <- uber_data %>% count(day, month)
by_wday_month <- uber_data %>% count(wday, month)
by_base_month <- uber_data %>% count(base, month)
hm_hour_wday  <- uber_data %>% count(hour, wday)
hm_month_day  <- uber_data %>% count(month, day)
hm_month_week <- uber_data %>% count(month, week)
hm_base_wday  <- uber_data %>% count(base, wday)

# UI Definition
ui <- navbarPage(
  "Uber Rides 2014",
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#2C3E50",
    secondary  = "#18BC9C",
    base_font  = font_google("Source Sans Pro"),
    code_font  = font_google("Fira Code")
  ),
  collapsible = TRUE,
  header = tagList(tags$style(HTML("body { padding-top: 70px; }"))),
  
  tabPanel("Overview",
           fluidRow(
             column(6, DTOutput("tbl_overview")),
             column(6, plotOutput("plot_month"))
           )
  ),
  tabPanel("Hourly",
           fluidRow(
             column(6, plotOutput("plot_hour")),
             column(6, plotOutput("plot_hour_month"))
           ),
           fluidRow(column(12, DTOutput("tbl_hour")))
  ),
  tabPanel("Daily",
           fluidRow(
             column(6, plotOutput("plot_day_of_month")),
             column(6, plotOutput("plot_wday_month"))
           ),
           fluidRow(column(12, DTOutput("tbl_day_month")))
  ),
  tabPanel("Base × Month",
           plotOutput("plot_base_month")
  ),
  tabPanel("Heatmaps",
           fluidRow(
             column(6, plotOutput("hm_hour_wday")),
             column(6, plotOutput("hm_month_day"))
           ),
           fluidRow(
             column(6, plotOutput("hm_month_week")),
             column(6, plotOutput("hm_base_wday"))
           )
  ),
  tabPanel("Map",
           leafletOutput("map", height = 600)
  )
)

# Server Logic
server <- function(input, output, session) {
  # Overview
  output$tbl_overview <- renderDT(datatable(by_month, colnames = c("Month","Trips")))
  output$plot_month   <- renderPlot({
    ggplot(by_month, aes(month, n)) +
      geom_col(fill = "steelblue") +
      labs(x = "Month", y = "Trips") +
      theme_minimal()
  })
  
  # Hourly
  output$plot_hour       <- renderPlot({
    ggplot(by_hour, aes(hour, n)) +
      geom_line(group = 1) +
      scale_y_continuous(labels = comma) +
      theme_minimal()
  })
  output$plot_hour_month <- renderPlot({
    ggplot(by_hour_month, aes(hour, n, color = month)) +
      geom_line() +
      theme_minimal()
  })
  output$tbl_hour <- renderDT(datatable(by_hour, colnames = c("Hour","Trips")))
  
  # Daily
  output$plot_day_of_month <- renderPlot({
    ggplot(by_day_month, aes(day, n, fill = month)) +
      geom_col(position = "dodge") +
      theme_minimal()
  })
  output$plot_wday_month   <- renderPlot({
    ggplot(by_wday_month, aes(wday, n, fill = month)) +
      geom_col(position = "dodge") +
      theme_minimal()
  })
  output$tbl_day_month <- renderDT(datatable(by_day_month, colnames = c("Day","Month","Trips")))
  
  # Base × Month
  output$plot_base_month <- renderPlot({
    ggplot(by_base_month, aes(base, n, fill = month)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = comma) +
      theme_minimal()
  })
  
  # Heatmaps
  heatmap_plot <- function(df, x, y, title) {
    ggplot(df, aes_string(x = x, y = y, fill = "n")) +
      geom_tile() +
      theme_minimal()
  }
  output$hm_hour_wday  <- renderPlot({ heatmap_plot(hm_hour_wday,  "hour",  "wday",        "Hour vs Weekday")  })
  output$hm_month_day  <- renderPlot({ heatmap_plot(hm_month_day,  "month", "day",        "Month vs Day")      })
  output$hm_month_week <- renderPlot({ heatmap_plot(hm_month_week, "month", "week",       "Month vs Week")     })
  output$hm_base_wday  <- renderPlot({ heatmap_plot(hm_base_wday,  "base",  "wday",      "Base vs Weekday") })
  
  # Map: kernel density via raster (sampled subset)
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

# Launch the application
shinyApp(ui, server)
