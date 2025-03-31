# Load necessary packages
pacman::p_load(sp, sf, raster, spatstat, tmap, tidyverse,
               spNetwork, tmaptools, raster, leaflet, patchwork, gridExtra,
               ggplot2, grid, terra, gstat, viridis, automap)
library(shiny)
library(sf)
library(raster)
library(spatstat)
library(tmap)
library(tidyverse)
library(spNetwork)
library(tmaptools)
library(raster)
library(leaflet)
library(patchwork)
library(gridExtra)
library(ggplot2)
library(grid)
library(terra)
library(gstat)
library(viridis)
library(automap)

# Read spatial data (assuming data is in RDS format)
monthly_weather <- readRDS("data/monthly_weather.rds")
monthly_rainfall <- readRDS("data/monthly_rainfall.rds")

available_months <- unique(monthly_rainfall$month)

# UI
ui <- fluidPage(
  titlePanel("Rainfall Map"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select a month to display max rainfall at different stations."),
      selectInput("selected_month", "Choose Month:", choices = available_months, selected = available_months[1])
    ),
    mainPanel(
      tmapOutput("rainfallMap")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$rainfallMap <- renderTmap({
    
    # Filter dataset based on selected month
    filtered_data <- monthly_rainfall %>% filter(month == input$selected_month)
    
    # Render tmap
    tmap_mode("view")
    tm_shape(filtered_data) +
      tm_symbols(
        fill = "max_rainfall",
        fill.scale = tm_scale("Blues"),
        size.legend = tm_legend_hide()
      ) +
      tm_title(paste(input$selected_month, "Rainfall (mm)")) +
      tm_shape(filtered_data) +
      tm_text("Station", size = 0.7, col = "black", ymod = -1)
  })
}



# Run the app
shinyApp(ui = ui, server = server)