library(shiny)
library(ggplot2)


ui <- fluidPage(
  sliderInput("format", "Custom Format:",
              min = 0, max = 10000,
              value = 0, step = 2500,
              pre = "$", sep = ",",
              animate = TRUE)
)

server <- function(input, output, session) {

}

shinyApp(ui, server)