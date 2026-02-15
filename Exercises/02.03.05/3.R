library(shiny)
library(ggplot2)


ui <- fluidPage(
  dataTableOutput("table")
)
#> `shiny::dataTableOutput()` is deprecated as of shiny 1.8.1.
#> Please use `DT::DTOutput()` instead.
#> See <https://rstudio.github.io/DT/shiny.html> for more information.
server <- function(input, output, session) {
  output$table <- renderDataTable(mtcars, options = list(pageLength = 5, searching = FALSE, ordering = FALSE, filtering = FALSE))
}

shinyApp(ui, server)