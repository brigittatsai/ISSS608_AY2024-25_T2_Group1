pacman::p_load(tmap, shiny)

tmap_mode("view")

ui <- fluidPage(
  titlePanel("Interactive tmap in Shiny"),
  tmapOutput("map", height = "600px")
)

server <- function(input, output, session) {
  output$map <- renderTmap({
    tm_shape(World) +
      tm_polygons("pop_est", id = "name", popup.vars = TRUE)
  })
}

shinyApp(ui, server)