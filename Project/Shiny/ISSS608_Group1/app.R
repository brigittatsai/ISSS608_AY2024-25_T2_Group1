pacman::p_load(sp, sf, raster, spatstat, tmap, tidyverse, plotly, dplyr,
               spNetwork, tmaptools, raster, leaflet, patchwork, gridExtra,
               ggplot2, grid, terra, gstat, viridis, automap, DT, shinydashboard)


monthly_weather <- readRDS("data/monthly_weather.rds")
weather <- read.csv("data/weather_clean.csv", stringsAsFactors = FALSE)
weather$date <- as.Date(weather$date, format = "%Y-%m-%d")

variable_titles <- list(
  "Frequency of Heavy Rain" = "Heavy Rain",
  "Frequency of Extreme Heat" = "Extreme Heat",
  "Frequency of Strong Wind" = "Strong Wind"
)

category_columns <- list(
  "Rainfall" = c("Maximum Rainfall", "Mean Rainfall", "Frequency of Heavy Rain"),
  "Wind" = c("Maximum Wind Speed", "Mean Wind Speed", "Frequency of Strong Wind"),
  "Temperature" = c("Maximum Temperature", "Mean Temperature", "Frequency of Extreme Heat")
)


variable_labels <- list(
  "Maximum Rainfall" = "Max",
  "Mean Rainfall" = "Mean",
  "Frequency of Heavy Rain" = "Frequency",
  "Maximum Wind Speed" = "Max",
  "Mean Wind Speed" = "Mean",
  "Frequency of Strong Wind" = "Frequency",
  "Maximum Temperature" = "Max",
  "Mean Temperature" = "Mean",
  "Frequency of Extreme Heat" = "Frequency"
)

rainfall_parameter <- data.frame(
  "Rain Type" = c("No Rain", "Very Light Rain", "Light Rain", "Moderate Rain", "Heavy Rain", "Very Heavy Rain", "Extremely Heavy Rain"),
  "Total Daily Rainfall (mm)" = c("0", "0.1 - 0.9", "1 - 10", "11 - 30", "31 - 70", "71 - 150", "> 151")
)

colnames(rainfall_parameter) <- c("Rain Type", "Total Daily Rainfall (mm)")

temp_parameter <- data.frame(
  "Heat Stress" = c("Low Heat Stress", "Moderate Heat Stress", "High Heat Stress"),
  "Temperature (°C)" = c("< 31", "31 ≤ °C < 33", "≥ 33")
)

colnames(temp_parameter) <- c("Heat Stress", "Temperature (°C)")

wind_parameter <- data.frame(
  "Wind Force" = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  "Description" = c("Calm", "Light Air", "Light Breeze", "Gentle Breeze", "Moderate Breeze", "Fresh Breeze", "Strong Breeze", "Near Gale", "Gale", "Strong Gale", "Storm", "Violent Storm", "Hurricane"),
  "Speed (km/h)" = c("< 1", "1 - 5", "6 - 11", "12 - 19", "20 - 28", "29 - 38", "38 - 49", "50 - 61", "62 - 74", "75 - 88", "89 - 102", "103 - 117", "≥ 118")
)

colnames(wind_parameter) <- c("Wind Force", "Description", "Speed (km/h)")


available_months <- unique(monthly_weather$month)


#====================
#  UI
#====================
ui <- navbarPage(
  "Weather Map",
  
  #------------------------------------------------
  #  (Map)
  #------------------------------------------------
  tabPanel(
    "Map",
    fluidPage(
      titlePanel("Weather Data Visualization"),
      textOutput("current_tab"),
      sidebarLayout(
        sidebarPanel(
          selectInput("selected_month", "Select Month (2024):", choices = available_months, selected = available_months[1]),
          conditionalPanel(
            condition = "input.tabs === 'Weather Map'",
            selectInput("selected_category", "Select Category:", choices = names(category_columns), selected = "Rainfall"),
            selectInput("selected_variable", "Select Measurement:", choices = NULL)
          ),
          conditionalPanel(
            condition = "input.tabs === 'Weather Frequency'",
            selectInput("selected_variable", "Select Frequency Variable:", 
                        choices = setNames(names(variable_titles), variable_titles),
                        selected = "frequency_heavy_rain")
          ),
          tableOutput("parameter_table")
        ),
        mainPanel(
          tabsetPanel(
            id = "tabs",
            tabPanel("Weather Map", tmapOutput("weather_map", height = "600px")),
            tabPanel("Weather Frequency", plotOutput("weather_plot", height = "600px"))
          )
        )
      )
    )
  ),
  
  #------------------------------------------------
  # (Explore)
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
  output$current_tab <- renderText({
    paste("Current Tab:", input$tabs)
  })
  
  observe({
    category_vars <- category_columns[[input$selected_category]]
    
    variable_choices <- setNames(category_vars, sapply(category_vars, function(x) variable_labels[[x]]))
    updateSelectInput(session, "selected_variable", 
                      choices = variable_choices,
                      selected = category_vars[1])
  })
  output$weather_map <- renderTmap({
    
    filtered_data <- monthly_weather %>% filter(month == input$selected_month)
    selected_variable <- input$selected_variable
    tmap_mode("view")
    tm_shape(filtered_data) +
      tm_symbols(
        fill = selected_variable,
        fill.scale = tm_scale("Blues"),
        size.legend = tm_legend_hide()
      ) +
      tm_title(paste(input$selected_month, selected_variable)) +
      tm_shape(filtered_data) +
      tm_text("Station", size = 0.7, col = "black", ymod = -1)
  })
  
  output$weather_plot <- renderPlot({
    selected_variable_title <- variable_titles[[input$selected_variable]]
    monthly_weather_filtered <- monthly_weather %>%
      filter(month == input$selected_month)
    ggplot(monthly_weather_filtered, aes(x = Station, y = .data[[input$selected_variable]], fill = Station)) +
      geom_bar(stat = "identity", fill="royalblue") +
      labs(
        title = paste("Frequency of", selected_variable_title, "in", input$selected_month),
        x = "Station", 
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5),
        strip.text = element_text(size = 12),
        legend.position = "none"
      ) +
      scale_x_discrete(expand = expansion(mult = c(0, 0))) +
      coord_cartesian(ylim = c(0, max(monthly_weather_filtered[[input$selected_variable]], na.rm = TRUE) * 1.2))
  })
  observe({
    updateSelectInput(session, "selected_variable", selected = "frequency_heavy_rain")
  })
  output$parameter_table <- renderTable({
    if (input$selected_variable == "frequency_heavy_rain") {
      rainfall_parameter
    } else if (input$selected_variable == "frequency_high_heat") {
      temp_parameter
    } else if (input$selected_variable == "frequency_strong_wind") {
      wind_parameter
    }
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
  output$table <- renderDT({S
    data <- filtered_data()
    req(nrow(data) > 0)
    
    datatable(data, options = list(pageLength = 10, scrollX = TRUE))
  })
}

#====================
# Shiny App
#====================
shinyApp(ui = ui, server = server)

