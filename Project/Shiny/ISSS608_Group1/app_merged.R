# 載入必要套件
library(shiny)
library(tmap)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinydashboard)  # 若要使用 box() 等佈局函數，需保留


map <- readRDS("data/map.rds")
daily_weather <- readRDS("data/daily_weather.rds")
jan_rainfall <- readRDS("data/jan_rainfall.rds")
monthly_rainfall <- readRDS("data/monthly_rainfall.rds")
monthly_temp <- readRDS("data/monthly_temp.rds")
monthly_wind <- readRDS("data/monthly_wind.rds")


weather <- read.csv("data/weather_clean.csv", stringsAsFactors = FALSE)
weather$date <- as.Date(weather$date, format = "%Y-%m-%d")
#====================
#  UI
#====================
ui <- navbarPage(
  "Rainfall App",
  
  #------------------------------------------------
  #  (Rainfall Map)
  #------------------------------------------------
  tabPanel(
    "Rainfall Map",
    
    fluidPage(
      titlePanel("Rainfall Map"),
      sidebarLayout(
        sidebarPanel(
          helpText("This map displays max rainfall in January at different stations.")
        ),
        mainPanel(
          tmapOutput("rainfallMap")
        )
      )
    )
  ),
  
  #------------------------------------------------
  # (Time Series & Data Table)
  #------------------------------------------------
  tabPanel(
    "Explore",
    
    fluidPage(
      # 使用 tabsetPanel 來區分「Time Series & Stats」與「Data Table」兩個子分頁
      tabsetPanel(
        
        #---- 子分頁 1: Time Series & Stats ----
        tabPanel("Time Series & Stats",
                 fluidRow(
                   box(
                     selectInput("station", "Select Station:",
                                 choices = unique(weather$station)),
                     width = 4
                   ),
                   box(
                     selectInput("variable", "Select Variable:",
                                 choices = c("daily_rainfall_total_mm",
                                             "highest_30_min_rainfall_mm",
                                             "highest_60_min_rainfall_mm",
                                             "highest_120_min_rainfall_mm",
                                             "mean_temperature_c",
                                             "maximum_temperature_c",
                                             "minimum_temperature_c",
                                             "mean_wind_speed_km_h",
                                             "max_wind_speed_km_h")),
                     width = 4
                   ),
                   box(
                     dateRangeInput("daterange", "Select Date Range:",
                                    start = min(weather$date),
                                    end   = max(weather$date)),
                     width = 4
                   )
                 ),
                 fluidRow(
                   box(
                     title = "Time Series Plot",
                     plotlyOutput("ts_plot"),
                     width = 12
                   )
                 ),
                 fluidRow(
                   box(
                     title = "Summary Stats",
                     verbatimTextOutput("summary_stats"),
                     width = 12
                   )
                 )
        ),
        
        #---- 子分頁 2: Data Table ----
        tabPanel("Data Table",
                 fluidRow(
                   box(
                     title = "Weather Data Table",
                     DTOutput("table"),
                     width = 12
                   )
                 )
        )
      )
    )
  )
)

#====================
#  Server
#====================
server <- function(input, output, session) {
  
  #--- (A) ：Rainfall Map ---
  output$rainfallMap <- renderTmap({
    tmap_mode("view")  # Interactive map mode
    
    tm_shape(map) +
      tm_layout(
        main.title = "Maximum Rainfall (mm) in January 2024",
        main.title.position = "center",
        main.title.size = 1.2,
        legend.position = c("RIGHT", "BOTTOM"),
        legend.title.size = 0.8,
        legend.text.size = 0.8,
        legend.outside = TRUE,
        legend.outside.position = "bottom",
        legend.frame = FALSE,
        frame = TRUE
      ) +
      tm_shape(monthly_rainfall) +
      tm_symbols(
        col = "max_rainfall",
        palette = "Blues",
        title.col = "Max Rainfall (mm)",
        popup.vars = c("Station", "max_rainfall"),  
        legend.size.show = FALSE
      ) +
      tm_text("Station", size = 0.7, col = "black", shadow = FALSE, ymod = -0.4) +
      tm_basemap("CartoDB.Positron")
  })
  
  #--- (B) ：Reactive ---
  filtered_data <- reactive({
    req(input$station, input$daterange) 
    
    weather %>%
      filter(
        station == input$station,
        date >= input$daterange[1],
        date <= input$daterange[2]
      )
  })
  
  #--- (C) Time Series Plot ---
  output$ts_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0) 
    
    p <- ggplot(data, aes(x = date, y = .data[[input$variable]])) +
      geom_line(color = "steelblue") +
      labs(
        title = paste("Time Series of", input$variable),
        x = NULL,
        y = input$variable
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  #--- (D) Summary Stats ---
  output$summary_stats <- renderPrint({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    summary(data[[input$variable]])
  })
  
  #--- (E) Data Table ---
  output$table <- renderDT({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    datatable(data, options = list(pageLength = 10, scrollX = TRUE))
  })
}

#====================
# Shiny App
#====================
shinyApp(ui = ui, server = server)

