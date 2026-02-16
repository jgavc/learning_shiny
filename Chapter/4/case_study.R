library(shiny)
library(vroom)
library(tidyverse)

dir.create("neiss")
#> Warning in dir.create("neiss"): 'neiss' already exists
download <- function(name) {
  url <- "https://raw.github.com/hadley/mastering-shiny/main/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
injuries
#> # A tibble: 255,064 × 10
#>   trmt_date    age sex   race  body_part   diag         location prod_code weight
#>   <date>     <dbl> <chr> <chr> <chr>       <chr>        <chr>        <dbl>  <dbl>
#> 1 2017-01-01    71 male  white Upper Trunk Contusion O… Other P…      1807   77.7
#> 2 2017-01-01    16 male  white Lower Arm   Burns, Ther… Home           676   77.7
#> 3 2017-01-01    58 male  white Upper Trunk Contusion O… Home           649   77.7
#> 4 2017-01-01    21 male  white Lower Trunk Strain, Spr… Home          4076   77.7
#> 5 2017-01-01    54 male  white Head        Inter Organ… Other P…      1807   77.7
#> 6 2017-01-01    21 male  white Hand        Fracture     Home          1884   77.7
#> # ℹ 255,058 more rows
#> # ℹ 1 more variable: narrative <chr>

products <- vroom::vroom("neiss/products.tsv")
products
#> # A tibble: 38 × 2
#>   prod_code title                            
#>       <dbl> <chr>                            
#> 1       464 knives, not elsewhere classified 
#> 2       474 tableware and accessories        
#> 3       604 desks, chests, bureaus or buffets
#> 4       611 bathtubs or showers              
#> 5       649 toilets                          
#> 6       676 rugs or carpets, not specified   
#> # ℹ 32 more rows

population <- vroom::vroom("neiss/population.tsv")
population
#> # A tibble: 170 × 3
#>     age sex    population
#>   <dbl> <chr>       <dbl>
#> 1     0 female    1924145
#> 2     0 male      2015150
#> 3     1 female    1943534
#> 4     1 male      2031718
#> 5     2 female    1965150
#> 6     2 male      2056625
#> # ℹ 164 more rows
#> 
selected <- injuries %>% filter(prod_code == 649)
nrow(selected)
#> [1] 2993
selected %>% count(location, wt = weight, sort = TRUE)
#> # A tibble: 6 × 2
#>   location                         n
#>   <chr>                        <dbl>
#> 1 Home                       99603. 
#> 2 Other Public Property      18663. 
#> 3 Unknown                    16267. 
#> 4 School                       659. 
#> 5 Street Or Highway             16.2
#> 6 Sports Or Recreation Place    14.8

selected %>% count(body_part, wt = weight, sort = TRUE)
#> # A tibble: 24 × 2
#>   body_part        n
#>   <chr>        <dbl>
#> 1 Head        31370.
#> 2 Lower Trunk 26855.
#> 3 Face        13016.
#> 4 Upper Trunk 12508.
#> 5 Knee         6968.
#> 6 N.S./Unk     6741.
#> # ℹ 18 more rows

selected %>% count(diag, wt = weight, sort = TRUE)
#> # A tibble: 20 × 2
#>   diag                       n
#>   <chr>                  <dbl>
#> 1 Other Or Not Stated   32897.
#> 2 Contusion Or Abrasion 22493.
#> 3 Inter Organ Injury    21525.
#> 4 Fracture              21497.
#> 5 Laceration            18734.
#> 6 Strain, Sprain         7609.
#> # ℹ 14 more rows
summary <- selected %>% 
  count(age, sex, wt = weight)
summary
#> # A tibble: 208 × 3
#>     age sex         n
#>   <dbl> <chr>   <dbl>
#> 1     0 female   4.76
#> 2     0 male    14.3 
#> 3     1 female 253.  
#> 4     1 male   231.  
#> 5     2 female 438.  
#> 6     2 male   632.  
#> # ℹ 202 more rows

summary %>% 
  ggplot(aes(age, n, colour = sex)) + 
  geom_line() + 
  labs(y = "Estimated number of injuries")

summary <- selected %>% 
  count(age, sex, wt = weight) %>% 
  left_join(population, by = c("age", "sex")) %>% 
  mutate(rate = n / population * 1e4)

summary
#> # A tibble: 208 × 5
#>     age sex         n population   rate
#>   <dbl> <chr>   <dbl>      <dbl>  <dbl>
#> 1     0 female   4.76    1924145 0.0247
#> 2     0 male    14.3     2015150 0.0708
#> 3     1 female 253.      1943534 1.30  
#> 4     1 male   231.      2031718 1.14  
#> 5     2 female 438.      1965150 2.23  
#> 6     2 male   632.      2056625 3.07  
#> # ℹ 202 more rows

summary %>% 
  ggplot(aes(age, rate, colour = sex)) + 
  geom_line(na.rm = TRUE) + 
  labs(y = "Injuries per 10,000 people")

selected %>% 
  sample_n(10) %>% 
  pull(narrative)
#>  [1] "24YOF BIB/EMS;GLF;DRINK@BRIDAL PARTY;BECAME SICK/THREW UP IN TOILET,STOOD UP;FELL HIT HEAD ON TOILET?LOC;C/O PAIN; DX:CLOSED HEAD INJURY"
#>  [2] "62 YOF FELL OFF TOILET LAST PM & TWISTED LEG.DX FX OF RT DISTAL FEMUR"                                                                   
#>  [3] "67 YOM STATES WAS ON TOILET AND REACHED BEHIND & FELT A POP IN UPPERARM  DX RIGHT UPPER ARM SPRAIN"                                      
#>  [4] "46 YOM C/O LOWER BACK PAIN AFTER TWISTING MOTION WHILE ON THE TOILETDX: LUMBAR STRAIN"                                                   
#>  [5] "84YOF WITH LOWER BACK FRACTURE AFTER FALLING WHILE ATTEMPTING TO SIT ONTOILET DX FRACTURE*"                                              
#>  [6] "82YOM WITH HEAD INJURY AFTER FALLING FROM TOILET STRIKING WALL DX HEADINJURY*"                                                           
#>  [7] "77 YR OLD FEMALE GETTING OFF TOILET AND FELL STRIKING BROW ON FLOOR ANDLAC FACE;DIZZINESS"                                               
#>  [8] "88YOF WITH HEAD INJURY AFTER FALLING FROM TOILET TO FLOOR DX HEAD INJURY*"                                                               
#>  [9] "90YOM PRESENTED PRESENTED TO ED FOR GLF AT NURSING FACILITY,FOUND IN RESTROOM,MAY HAVE BEEN ON TOILET.DX:GLF,FOREHEAD ABRASION"          
#> [10] "43 YO M SYNCOPE+COLLAPSE WHILE SITTING ON COMMODE"
#> 
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

prod_codes <- setNames(products$prod_code, products$title)

ui <- fluidPage(
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, sliderInput("row_num","How many rows", value = 5, min = 1, max = 10))
  ),
  fluidRow(
    column(2, actionButton("story_forward", "Tell me a new story")),
    column(2, actionButton("story_backward", "Tell me a previous story")),
    column(10, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  nar_length <- reactive(selected() |> pull(narrative) |> length())
  counter <- reactiveVal(1)
  observeEvent(input$story_forward, ifelse((counter() + 1) == nar_length(), counter(1), counter(counter() + 1)))
  observeEvent(input$story_backward, ifelse((counter() - 1) == 0, counter(nar_length()), counter(counter() - 1)))

  # observeEvent(
  #   list(input$story_forward, selected()),
  #   default_narrative_index(default_narrative_index() + 1)
  # )
  
  output$diag <- renderTable(count_top(selected(), diag, n = input$row_num), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part, n = input$row_num), width = "100%")
  output$location <- renderTable(count_top(selected(), location, n = input$row_num), width = "100%")
  narrative_sample <- eventReactive(
    list(input$story_forward, selected(), input$story_backward),
    selected() %>% slice(counter()) |> pull(narrative)
  )
  output$narrative <- renderText(narrative_sample())
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
}

shinyApp(ui,server)
