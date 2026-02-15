library(shiny)
library(ggplot2)


ui <- fluidPage(
selectInput("selection","What selection?", choices = c("test" = "test",
                                                       "test2" = "test2"))
)

server <- function(input, output, session) {

}

shinyApp(ui, server)