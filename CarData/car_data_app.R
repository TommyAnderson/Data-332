library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(rsconnect)
library(readxl)
library(RCurl)
library(lubridate)

rm(list = ls())

# Load and clean data

data_group1 <- getURL("https://raw.githubusercontent.com/TommyAnderson/Car-Data-Analysis/refs/heads/main/Car%20Data%20Collection.csv")
data_group2 <- getURL("https://raw.githubusercontent.com/retflipper/DATA332_CountingCars/refs/heads/main/data/Counting_Cars.csv")
data_group3 <- getURL("https://raw.githubusercontent.com/nissou62/The-very-basics-of-R/refs/heads/main/shinymtcar_project/Data_Counting_Cars.csv")
data_group4 <- getURL("https://raw.githubusercontent.com/nickhc41703/Data_332_assignments/refs/heads/main/Homework/counting_cars/counting_cars_final.csv")
data_group5 <- getURL("")
data_group6 <- getURL("")
data_group7 <- getURL("")

dataset <- read.csv(text = data_url, stringsAsFactors = FALSE)


# Standardize color labels
dataset$Color <- trimws(tolower(dataset$Color))
dataset$Color[dataset$Color %in% c("light grey", "dark grey", "grey")] <- "grey"
dataset$Color[dataset$Color %in% c("red", "maroon", "dark red")] <- "red"

# Standardize all text to lowercase
dataset <- dataset %>%
  mutate(across(where(is.character), tolower))

#convert to military time
dataset$Time <- parse_date_time(dataset$Time, orders = "I:M p")
dataset$TimeFormatted <- format(dataset$Time, "%H:%M")
dataset$Hour <- hour(dataset$Time)

# UI
column_names <- colnames(dataset)

ui <- fluidPage(
  titlePanel("Car Data For Rock Island"),
  h4("For Rock Island, IL"),
  tags$p(tags$b("Note:"), "Y-axis is always Speed"),
  
  fluidRow(
    column(2,
           selectInput('X', 'Choose X', column_names, selected = column_names[1]),
           selectInput('Splitby', 'Split By', column_names, selected = column_names[3])
    ),
    column(4,
           plotOutput('plot_01'),
           br(),
           textOutput("plot_description")
    ),
    column(6,
           DT::dataTableOutput("table_01", width = "100%")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$plot_01 <- renderPlot({
    x_data <- dataset[[input$X]]
    y_data <- dataset[["Speed"]]  # Fixed Y
    
    is_x_cat <- is.character(x_data) || is.factor(x_data) ||input$X == 'Hour'
    
    # Build appropriate plot
    if (is_x_cat) {
      ggplot(dataset, aes_string(x = input$X, y = "Speed", fill = input$Splitby)) +
        stat_summary(fun = mean, geom = "bar", position = "dodge") +
        ylab("Mean Speed")
    } else {
      ggplot(dataset, aes_string(x = input$X, y = "Speed", colour = input$Splitby)) +
        geom_point() +
        ylab("Speed")
    }
  })
  

  output$plot_description <- renderText({
    if (input$X == "Hour") {
      "Average speed by hour of day. If enough data is collected should help see rush hour traffic"
    } else if (input$X == "Color") {
      "Comparing seed by car color, should show if car color has any type of correlation with speed"
    } else if (input$X == "Type.Of.Car") {
      "This bar chart shows average speed for different car types. It can suggest whether certain vehicle categories tend to drive faster."
    } else if (is.numeric(dataset[[input$X]])) {
      paste("This scatter plot shows how Speed changes in relation to", input$X, ", colored by", input$Splitby, ".")
    } else {
      paste("This bar chart displays average Speed by", input$X, ", grouped by", input$Splitby, ".")
    }
  })
  
  
  output$table_01 <- DT::renderDataTable({
    DT::datatable(dataset[, c(input$X, "Speed", input$Splitby)],
                  options = list(pageLength = 4))
  })
}

# Launch app
shinyApp(ui = ui, server = server)
