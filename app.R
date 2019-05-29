library(shiny)
library(magrittr)
library(dplyr)
library(tidyr)
library(wordcloud)
library(RColorBrewer)

wine.ratings <- readRDS("wine.ratings.rds")

words.scores <- readRDS("words.scores.rds")

word.occurences <- readRDS("words.occurences.rds")

ui <- fluidPage(

  # App title ----
  titlePanel("How words in wine description match the wine quality")

  , fluidRow(
    
    column(
      4
      , wellPanel(
        tags$head(tags$script('
$(document).on("shiny:connected", function(e) {
  Shiny.onInputChange("innerWidth", window.innerWidth);
});
$(window).resize(function(e) {
  Shiny.onInputChange("innerWidth", window.innerWidth);
});
'))
        # Input: Select rating range
        , sliderInput("points.range", "Wine rating range (relative)", min = 0, max = 100, value = c(90, 100))
        # Number of words in cloud
        , sliderInput("num.words", "Number of words to display in cloud", min = 10, max = 200, value = 50, step = 1)
        # Filter out most frequent words
        , sliderInput(
          "words.freq.cutoff"
          , "Words that occur in too many descriptions (percent of them) are excluded from cloud"
          , min = 0, max = 100, value = 20
        )
      )
    ),
    
    column(
      8
      , id = "main"
      , div("The word cloud below shows the most frequent words used to describe the selected fraction of wines in dataset.")
      , textOutput("points.range")
      , plotOutput("tag.cloud", width = "auto", height = "auto")
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  points.quantiles <- reactive({
    quantile(wine.ratings$points, input$points.range/100)
  })
  output$points.range <- renderText({
    paste0("Wines with rating from ", points.quantiles()[1], " to ", points.quantiles()[2], " are selected.")
  })
  count.words <- reactive({
    words.scores %>%
      filter(!words %in% (word.occurences %>% filter(rate.unique > (input$words.freq.cutoff/100)) %>% pull(words))) %>%
      filter(points >= points.quantiles()[1] & points <= points.quantiles()[2]) %>%
      pull(words) %>%
      table() %>%
      sort()
  })
  colors <- reactive({
    colorRampPalette( brewer.pal( 12, "Paired" ) )( input$num.words )
  })
  output$tag.cloud <- renderPlot(
    {
      wordcloud(
        names(count.words()) %>% tail(input$num.words)
        , count.words() %>% tail(input$num.words)
        , colors = colors()
        , scale =       case_when(
          is.null(input[["innerWidth"]]) ~ c(4, .5)
          , input[["innerWidth"]] < 768  ~ input[["innerWidth"]] / c(167, 1067)
          , TRUE                         ~ input[["innerWidth"]] / c(200, 1600)
        )
        
      )
    }
    # , height = reactive(ifelse(!is.null(input$innerWidth), (input$innerWidth * .8) %>% as.character(), "auto"))
    , height = reactive(
      case_when(
        is.null(input[["innerWidth"]]) ~ 0
        , input[["innerWidth"]] < 768  ~ input$innerWidth * .9
        , input[["innerWidth"]] < 992  ~ input$innerWidth * .55
        , TRUE                         ~ input$innerWidth * .55
      )
    )
  )

}

shinyApp(ui, server)