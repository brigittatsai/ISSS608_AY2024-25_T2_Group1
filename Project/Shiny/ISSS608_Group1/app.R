pacman::p_load(shiny, tidyverse, rsconnect)
library(shiny)
library(tidyverse)
library(rsconnect)
library(readxl)
library(data.table)

folder_path <- "ISSS608_Group1/data/aspatial/weather"
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read and combine all Excel files using data.table
combined_data <- rbindlist(lapply(file_list, fread))

# View the merged dataframe
head(combined_data)


ui <- fluidPage(
  titlePanel("Pupil Exam Results Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable",
                  label = "Subject:",
                  choices = c("English" = "ENGLISH",
                              "Maths" = "MATHS",
                              "Science" = "SCIENCE"),
                  selected = "ENGLISH"),
      sliderInput(inputId = "bins",
                  label = "Number of Bins",
                  min = 5,
                  max = 20,
                  value = 10)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output){
  output$distPlot <- renderPlot({
    ggplot(exam,
           aes_string(x = input$variable)) +
      geom_histogram(bins = input$bins,
                     color = "black",
                     fill = "lightblue")
  })
}

shinyApp(ui = ui, server = server)