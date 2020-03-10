library(shiny)

# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Median Points Scored and Allowed"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    selectInput("variable", "Team Name:", 
                c("Cylinders" = "cyl",
                  "Transmission" = "am",
                  "Gears" = "gear"))
  ),
  
  # Main panel for displaying outputs ----
  mainPanel()
)

server <- function(input, output) {
  
}

shinyApp(ui, server)