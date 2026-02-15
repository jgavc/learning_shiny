library(shiny)
library(ggplot2)


ui <- fluidPage(
textInput("name","", value = "Your name")
)

server <- function(input, output, session) {

}

shinyApp(ui, server)