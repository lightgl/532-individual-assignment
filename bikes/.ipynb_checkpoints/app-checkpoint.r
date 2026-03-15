library(shiny)
library(dplyr)
library(lubridate)
library(plotly)
library(readr)

# Load data
df <- read_csv("../201306-citibike-tripdata.csv")

df <- df %>%
  mutate(
    starttime = ymd_hms(starttime),
    stoptime = ymd_hms(stoptime),
    start_hour = hour(starttime)
  )

ui <- fluidPage(
  
  tags$style("body { font-size: 3em; }"),
  
  titlePanel("Citi Bikes"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(
        "start_time_slider",
        "Start Hour",
        min = 0,
        max = 23,
        value = c(0, 23)
      )
    ),
    
    mainPanel(
      
      fluidRow(
        textOutput("pop_start_hour")
      ),
      
      fluidRow(
        plotlyOutput("start_hour_barplot")
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  filtered_df <- reactive({
    
    s_min <- input$start_time_slider[1]
    s_max <- input$start_time_slider[2]
    
    df %>%
      filter(start_hour >= s_min & start_hour <= s_max)
  })
  
  output$pop_start_hour <- renderText({
  d <- filtered_df()
  
  if (nrow(d) == 0) {
    "N/A"
  } else {
    start_hour <- as.numeric(names(sort(table(d$start_hour), decreasing = TRUE)[1]))
    paste("Most Popular Start Hour:", start_hour)
  }
})
  
  output$start_hour_barplot <- renderPlotly({
    
    d <- filtered_df()
    
    trips_per_start_hour <- d %>%
      group_by(start_hour) %>%
      summarise(trip_count = n(), .groups = "drop")
    
    plot_ly(
      trips_per_start_hour,
      x = ~start_hour,
      y = ~trip_count,
      type = "bar"
    )
  })
  
}

shinyApp(ui, server)