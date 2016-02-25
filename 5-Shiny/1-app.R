library(shiny)
library(ggplot2)

ui <- fluidPage(
  numericInput(
    inputId = "size",
    label = "Choose a point size",
    value = 3,
    min = 1,
    max = 10
  ),
  plotOutput("plotId")
)

server <- function(input, output) {
  output$plotId <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) + 
      geom_point(size = input$size)
  }) 
}

shinyApp(ui, server)
