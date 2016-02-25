library(shiny)
library(ggplot2)

ui <- fluidPage(
  sidebarPanel(
    selectInput(
      inputId = "x",
      label = "Choose an x variable",
      choices = names(mtcars)
    ),
    selectInput(
      inputId = "y",
      label = "Choose an y variable",
      choices = names(mtcars)
    )
  ),
  mainPanel(
    plotOutput("plotId")
  )
)

server <- function(input, output) {
  output$plotId <- renderPlot({
    ggplot(mtcars, aes_string(input$x, input$y)) + 
      geom_point()
  }) 
}

shinyApp(ui, server)
