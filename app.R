library(shiny)
library(shinydashboard)
library(magrittr)
library(dplyr)
library(tidyr)
library(tagcloud)
library(RColorBrewer)


words.scores <- readRDS("words.scores.rds")

word.occurences <- readRDS("words.occurences.rds")

ui <- fluidPage(

  # App title ----
  titlePanel("Wine Description keywords")

  # Sidebar panel for inputs ----
  , sidebarLayout(
    sidebarPanel(
      # Input: Select rating range
      sliderInput("points.range", "Wine rating range (relative)", min = 0, max = 100, value = c(90, 100))
      # Number of words in cloud
      , sliderInput("num.words", "Number of keywords", min = 10, max = 200, value = 50, step = 1)
      # Filter out most frequent words
      , sliderInput("words.freq.cutoff", "Remove words occuring in more than % descriptions", min = 0, max = 100, value = 15)
    )
    , mainPanel(
      fillPage(plotOutput("tag.cloud"))
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  points.quantiles <- reactive({
    quantile(wine.ratings$points, input$points.range/100)
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
      tagcloud(names(count.words()) %>% tail(input$num.words), count.words() %>% tail(input$num.words), col = colors())
    }
    , width = 500, height = 500
  )

}

shinyApp(ui, server)