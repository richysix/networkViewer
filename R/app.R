library(shiny)
library(r2d3)

myApp <- function(testing = FALSE, debug = FALSE) {
  ui <- fluidPage(
    theme = "flatly.bootstrap.min.css",
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/graph.css")
    ),
    navbarPage(
      "Cluster Viewer",
      tabPanel("Data",
               sidebarLayout(
                 sidebarPanel(
                   # File upload
                   uploadGraphInput("GraphData")
                 ),
                 mainPanel(
                   # file output for testing
                   tableOutput("nodes"),
                   tableOutput("edges")
                 )
               )
      )
    )
  )
  server <- function(input, output, session) {
    data_list <- uploadGraphServer("GraphData", debug)
    output$nodes <- renderTable(data_list$nodes()[1:5,])
    output$edges <- renderTable(data_list$edges()[1:5,])
  }
  shinyApp(ui, server)
}
