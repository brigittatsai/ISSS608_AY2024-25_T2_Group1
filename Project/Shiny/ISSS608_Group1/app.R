# Load necessary packages
pacman::p_load(shiny, tidyverse, tmap, sf)

# Read spatial data (assuming data is in RDS format)
daily_weather <- readRDS("data/daily_weather.rds")
jan_rainfall_with_data <- readRDS("data/jan_rainfall_with_data.rds")

# Ensure jan_rainfall_with_data is an sf object (modify coords if necessary)
if (!inherits(jan_rainfall_with_data, "sf")) {
  jan_rainfall_with_data <- st_as_sf(jan_rainfall_with_data, coords = c("longitude", "latitude"), crs = 4326)
}

# UI
ui <- fluidPage(
  titlePanel("January Max Rainfall Map"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("This map displays max rainfall in January at different stations.")
    ),
    
    mainPanel(
      tmapOutput("rainfallMap")  # Use tmapOutput instead of plotOutput
    )
  )
)

# Server
server <- function(input, output, session) {
  output$rainfallMap <- renderTmap({
    tmap_mode("view")  # Ensure interactive mode
    
    tm_shape(jan_rainfall_with_data) +
      tm_symbols(
        col = "max_rainfall", 
        size = "max_rainfall", 
        palette = "Blues",
        title.col = "Max Rainfall (mm)",
        popup.vars = c("Station", "max_rainfall"),
        legend.size.show = FALSE
      ) +
      tm_layout(title = "January Max Rainfall (mm)", legend.outside = TRUE) +
      tm_shape(jan_rainfall_with_data) +
      tm_text("Station", size = 0.7, col = "black",
              shadow = FALSE, ymod = -1)
  })
}


shinyApp(ui = ui, server = server)