library(shiny)
library(reactable)

source("~/New CFB.R")
source("~/GitHub/Sports-Data/College Football/ELO Ratings/Bowl Games.R")
source("~/GitHub/Sports-Data/College Football/ELO Ratings/Predict Games.R")
source("~/GitHub/Sports-Data/College Football/ELO Ratings/Table Output.R")

ui <- fluidPage(
  titlePanel("reactable example"),
  reactableOutput("table")
)

server <- function(input, output, session) {
  output$table <- renderReactable({
    withtitle
  })
}

shinyApp(ui, server)