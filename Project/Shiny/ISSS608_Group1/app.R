pacman::p_load(sf, tmap, tidyverse, plotly, dplyr, tmaptools, leaflet, shinyWidgets, reshape2,
               ggplot2, DT, shinydashboard, fable, fabletools, lubridate, zoo, tsibble, forecast)

# 1. Variables set for Weather Map
# Set Interactive Mode
tmap_mode("view")

monthly_weather <- readRDS("data/monthly_weather.rds")
weather <- read_csv("data/weather_clean.csv")
weather$date <- as.Date(weather$date, format = "%Y-%m-%d")
# ts_data <- read_csv("data/ts_data.csv")
# ts_data_weekly <- read_csv("data/ts_data_weekly.csv")

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


choices <- names(variable_labels)
selected_var <- variable_labels[["Maximum Rainfall"]]  # This will return "Max"

print(choices)

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

# 2. Variables set for forecasting module

# Sample data generation (replace this with your actual data loading)
set.seed(123)
stations <- c("Admiralty", "Ang Mo Kio", "Changi", "Choa Chu Kang (South)", "Clementi",
              "East Coast Parkway", "Jurong (West)", "Jurong Island", "Newton", "Pasir Panjang",
              "Paya Lebar", "Pulau Ubin", "Seletar", "Sembawang", "Sentosa Island",
              "Tai Seng", "Tuas South")
ts_data <- tibble(
  date = rep(seq.Date(from = as.Date('2023-01-01'), by = 'day', length.out = 365), 17),
  station = rep(stations, each = 365),
  mean_temperature_c = rnorm(365 * 17, 30, 2)
)

# Prepare the data and split into Training/Hold-out sets for each station
ts_data <- ts_data %>%
  mutate(Type = if_else(date >= "2024-10-01", "Hold-out", "Training"))

# Sample weather station names
station_names <- c("Admiralty", "Ang Mo Kio", "Changi", "Choa Chu Kang (South)", "Clementi",
                   "East Coast Parkway", "Jurong (West)", "Jurong Island", "Newton", "Pasir Panjang",
                   "Paya Lebar", "Pulau Ubin", "Seletar", "Sembawang", "Sentosa Island",
                   "Tai Seng", "Tuas South")

# Create sample daily data
set.seed(123)  # Set seed for reproducibility
ts_data <- tibble(
  station = rep(station_names, each = 365),  # 17 stations, each with 365 entries
  date = rep(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), length.out = 365 * 17),
  mean_temperature = rnorm(365 * 17, 28, 2),
  min_temperature = rnorm(365 * 17, 26, 2),
  max_temperature = rnorm(365 * 17, 31, 2),
  total_rainfall = rnorm(365 * 17, 5, 2)
) %>%
  as_tsibble(index = date, key = station)

# Introduce missing values randomly (simulating missing data)
set.seed(456)  # For reproducibility of missing values
missing_indices <- sample(1:nrow(ts_data), size = floor(0.05 * nrow(ts_data)))  # 5% missing
ts_data[missing_indices, "mean_temperature"] <- NA
ts_data[missing_indices, "min_temperature"] <- NA
ts_data[missing_indices, "max_temperature"] <- NA
ts_data[missing_indices, "total_rainfall"] <- NA

# Fill missing values with the mean of the respective column
ts_data_filled <- ts_data %>%
  mutate(
    mean_temperature = if_else(is.na(mean_temperature), mean(mean_temperature, na.rm = TRUE), mean_temperature),
    min_temperature = if_else(is.na(min_temperature), mean(min_temperature, na.rm = TRUE), min_temperature),
    max_temperature = if_else(is.na(max_temperature), mean(max_temperature, na.rm = TRUE), max_temperature),
    total_rainfall = if_else(is.na(total_rainfall), mean(total_rainfall, na.rm = TRUE), total_rainfall)
  )

# Split into Training and Hold-out sets
ts_data_split <- ts_data_filled %>%
  mutate(Type = if_else(date >= "2024-10-01", "Hold-out", "Training")) %>%
  as_tsibble(index = date, key = station)


#====================
#  UI
#====================
ui <- navbarPage(
  title = div(icon("cloud-sun"), "Singapore's 2024 Weather Data Visualization"),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f7fa;
        color: #333;
      }
      .navbar {
        background-color: #007acc;
      }
      .navbar .navbar-brand,
      .navbar-nav > li > a {
        color: white !important;
      }
      .navbar-nav > li > a:hover {
        background-color: #005f99 !important;
      }
      .selectize-input, .selectize-dropdown {
        background-color: #e6f2ff;
        color: #003366;
      }
      .sidebar-panel {
        background-color: #e3f2fd;
      }
      .btn-primary {
        background-color: #005f99;
        border-color: #005f99;
      }
      .btn-primary:hover {
        background-color: #3399cc;
      }
      .box {
        border-top: 3px solid #007acc;
      }
    "))
  ),
  
  #------------------------------------------------
  # 🌍 Map
  #------------------------------------------------
  tabPanel(
    title = tagList(icon("map-marked-alt"), "Map"),
    fluidPage(
      titlePanel("Weather Data Visualization"),
      textOutput("current_tab"),
      sidebarLayout(
        sidebarPanel(
          h4(icon("calendar-alt"), "Select Month"),
          selectInput("selected_month", "Month (2024):", choices = available_months, selected = available_months[1]),
          hr(),
          conditionalPanel(
            condition = "input.tabs === 'Weather Map'",
            h4(icon("cloud"), "Map Options"),
            selectInput("selected_category", "Category:", choices = names(category_columns), selected = "Rainfall"),
            selectInput("selected_variable", "Measurement:", 
                        choices = c("Max", "Mean", "Frequency"), selected = "Max")
          ),
          conditionalPanel(
            condition = "input.tabs === 'Weather Frequency'",
            h4(icon("chart-bar"), "Frequency Options"),
            selectInput("selected_variable", "Variable:", 
                        choices = setNames(names(variable_titles), variable_titles),
                        selected = "Frequency of Heavy Rain")
          ),
          br(),
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
  # 📊 Explore
  #------------------------------------------------
  tabPanel(
    title = tagList(icon("chart-line"), "Time Series Analysis"),
    fluidPage(
      tabsetPanel(
        tabPanel("Time Series & Stats",
                 fluidRow(
                   box(
                     title = tagList(icon("map-marker-alt"), "Station Selection"),
                     selectInput("station", "Station:", choices = unique(weather$station)),
                     width = 4
                   ),
                   box(
                     title = tagList(icon("cogs"), "Variable"),
                     selectInput("variable", "Variable:",
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
                     title = tagList(icon("calendar-day"), "Date Range"),
                     dateRangeInput("daterange", "Range:",
                                    start = min(weather$date),
                                    end = max(weather$date)),
                     width = 4
                   )
                 ),
                 fluidRow(
                   box(
                     title = tagList(icon("chart-line"), "Time Series Plot"),
                     plotlyOutput("ts_plot"),
                     width = 12
                   )
                 ),
                 fluidRow(
                   box(
                     title = tagList(icon("calculator"), "Summary Stats"),
                     verbatimTextOutput("summary_stats"),
                     width = 12
                   )
                 )
        ),
        tabPanel("Data Table",
                 fluidRow(
                   box(
                     title = tagList(icon("table"), "Weather Data Table"),
                     DTOutput("table"),
                     width = 12
                   )
                 )
        )
      )
    )
  ),
  
  #------------------------------------------------
  # 🔮 Univariate Forecasting
  #------------------------------------------------
  tabPanel(
    title = tagList(icon("chart-area"), "Forecasting"),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4(icon("map-pin"), "Station"),
          selectInput("station", "Weather Station:", choices = unique(ts_data$station)),
          h4(icon("sliders-h"), "Variable"),
          selectInput("variable", "Variable:", 
                      choices = c("Mean Temperature" = "mean_temperature",
                                  "Minimum Temperature" = "min_temperature",
                                  "Maximum Temperature" = "max_temperature",
                                  "Total Rainfall (mm)" = "total_rainfall")),
          h4(icon("calendar-range"), "Date & Forecast"),
          dateRangeInput("forecast_date_range", "Date Range:", 
                         start = min(ts_data$date), end = max(ts_data$date)),
          sliderInput("forecast_days", "Forecast Days:", min = 7, max = 100, value = 30),
          h4(icon("project-diagram"), "Model"),
          selectInput("forecast_model", "Model:",
                      choices = c("ETS", "ARIMA", "Holt's Linear", "SES", "Multi-ETS")),
          actionButton("generate_forecast", "Generate Forecast", icon = icon("play"))
        ),
        mainPanel(
          h3(icon("clipboard-check"), "Model Calibration Report"),
          tableOutput("model_report"),
          plotOutput("calibration_plot"),
          
          h3(icon("chart-line"), "Forecast Result"),
          plotOutput("forecast_plot"),
          tableOutput("forecast_table")
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
    
    filtered_month <- monthly_weather %>% filter(month == input$selected_month)
    selected_variable <- input$selected_variable
    
    tm_shape(filtered_month) +
      tm_symbols(
        fill = selected_variable,
        fill.scale = tm_scale("Blues"),
        size.legend = tm_legend_hide()
      ) +
      tm_title(paste(input$selected_month, selected_variable)) +
      tm_shape(filtered_month) +
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
    updateSelectInput(session, "selected_variable", selected = "Frequency of Heavy Rain")
  })
  output$parameter_table <- renderUI({
    if (input$selected_variable == "Frequency of Heavy Rain") {
      df <- rainfall_parameter
      rows <- lapply(1:nrow(df), function(i) {
        row <- df[i, ]
        is_bold <- row[["Rain Type"]] == "Heavy Rain"
        tags$tr(
          tags$td(style = if (is_bold) "font-weight:bold;" else "", row[["Rain Type"]]),
          tags$td(style = if (is_bold) "font-weight:bold;" else "", row[["Total Daily Rainfall (mm)"]])
        )
      })
      tags$table(
        class = "table table-bordered",
        tags$thead(tags$tr(
          tags$th("Rain Type"),
          tags$th("Total Daily Rainfall (mm)")
        )),
        tags$tbody(rows)
      )
      
    } else if (input$selected_variable == "Frequency of Extreme Heat") {
      df <- temp_parameter
      rows <- lapply(1:nrow(df), function(i) {
        row <- df[i, ]
        is_bold <- row[["Heat Stress"]] == "High Heat Stress"
        tags$tr(
          tags$td(style = if (is_bold) "font-weight:bold;" else "", row[["Heat Stress"]]),
          tags$td(style = if (is_bold) "font-weight:bold;" else "", row[["Temperature (°C)"]])
        )
      })
      tags$table(
        class = "table table-bordered",
        tags$thead(tags$tr(
          tags$th("Heat Stress"),
          tags$th("Temperature (°C)")
        )),
        tags$tbody(rows)
      )
      
    } else if (input$selected_variable == "Frequency of Strong Wind") {
      df <- wind_parameter
      rows <- lapply(1:nrow(df), function(i) {
        row <- df[i, ]
        is_bold <- row[["Description"]] == "Near Gale"
        tags$tr(
          tags$td(style = if (is_bold) "font-weight:bold;" else "", row[["Wind Force"]]),
          tags$td(style = if (is_bold) "font-weight:bold;" else "", row[["Description"]]),
          tags$td(style = if (is_bold) "font-weight:bold;" else "", row[["Speed (km/h)"]])
        )
      })
      tags$table(
        class = "table table-bordered",
        tags$thead(tags$tr(
          tags$th("Wind Force"),
          tags$th("Description"),
          tags$th("Speed (km/h)")
        )),
        tags$tbody(rows)
      )
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
  output$table <- renderDT({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    datatable(data, options = list(pageLength = 10, scrollX = TRUE))
  })
  

# Forecasting section
observeEvent(input$generate_forecast, {
  req(input$station, input$variable, input$forecast_days)
  
  # Filter the training data for the selected station
  train_data <- ts_data_split %>%
    filter(station == input$station, Type == "Training") %>%
    fill_gaps()
  
  # Fill gaps and impute missing values
  train_data <- train_data %>%
    mutate(variable_value = zoo::na.approx(!!sym(input$variable), na.rm = FALSE))
  
  # Fit the selected model
  model <- switch(input$forecast_model,
                  "ETS" = train_data %>% model(ETS(variable_value)),
                  "ARIMA" = train_data %>% model(ARIMA(variable_value)),
                  "Holt's Linear" = train_data %>% model(ETS(variable_value ~ trend("A"))),
                  "SES" = train_data %>% model(SNAIVE(variable_value)),
                  "Multi-ETS" = train_data %>% model(ETS(variable_value ~ error("A") + trend("A") + season("A")))
  )
  
  # Forecasting the future values
  forecast_result <- model %>%
    forecast(h = input$forecast_days)
  
  # Forecast plot
  output$calibration_plot <- renderPlot({
    autoplot(forecast_result, train_data) +
      ggtitle(paste("Forecast for", input$variable, "at", input$station)) +
      theme_minimal()
  })
  
  train_data <- ts_data_split %>%
    filter(station == input$station, Type == "Training") %>%
    fill_gaps()  # Filling missing dates if any
  
  # Impute missing values
  train_data <- train_data %>%
    mutate(variable_value = zoo::na.approx(!!sym(input$variable), na.rm = FALSE))
  
  # Fit the selected model
  model <- switch(input$forecast_model,
                  "ETS" = train_data %>% model(ETS(variable_value)),
                  "ARIMA" = train_data %>% model(ARIMA(variable_value)),
                  "Holt's Linear" = train_data %>% model(ETS(variable_value ~ trend("A"))),
                  "SES" = train_data %>% model(SNAIVE(variable_value)),
                  "Multi-ETS" = train_data %>% model(ETS(variable_value ~ error("A") + trend("A") + season("A")))
  )
  
  # Forecast future values
  forecast_result <- model %>% forecast(h = input$forecast_days)
  
  # Plot forecast
  output$forecast_plot <- renderPlot({
    autoplot(forecast_result) +
      ggtitle(paste("Forecast for", input$variable, "at", input$station)) +
      theme_minimal()
  })
  
  
  # Forecast table
  output$forecast_table <- renderTable({
    forecast_result %>%
      as_tibble() %>%
      select(date, .mean) %>%
      rename(Forecasted_Value = .mean) %>%
      mutate(date = format(as.Date(date), "%d %b %Y")) %>%
      mutate(Station = input$station) %>%
      select(Station, date, Forecasted_Value)
  })
})

}

#====================
# Shiny App
#====================
shinyApp(ui = ui, server = server)

