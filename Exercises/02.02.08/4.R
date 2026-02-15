library(shiny)
library(ggplot2)


ui <- fluidPage(
selectInput("selection","What selection?", choices = list('section 1' = list('1','2','3'),
                                                          'section 2' = list('4','5','6')))
)

server <- function(input, output, session) {

}

shinyApp(ui, server)