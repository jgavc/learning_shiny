library(shiny)
library(ggplot2)


ui <- fluidPage(
sliderInput("date","When should we deliver", value = as.Date("2020/09/17"), min = as.Date("2020/09/16"), max = as.Date("2020/09/23"))
)

server <- function(input, output, session) {

}

shinyApp(ui, server)