# Load necessary packages
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
map <- readRDS("data/map.rds")
daily_weather <- readRDS("data/daily_weather.rds")
jan_rainfall <- readRDS("data/jan_rainfall.rds")
monthly_rainfall <- readRDS("data/monthly_rainfall.rds")
monthly_temp <- readRDS("data/monthly_temp.rds")
monthly_wind <- readRDS("data/monthly_wind.rds")

# UI
ui <- fluidPage(
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

# Server
server <- function(input, output, session) {
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
        popup.vars = c("Station", "max_rainfall"),  # Make sure column names are correct
        legend.size.show = FALSE
      ) +
      tm_text("Station", size = 0.7, col = "black", shadow = FALSE, ymod=-0.4) +
      tm_basemap("CartoDB.Positron")
  })
}




shinyApp(ui = ui, server = server)
