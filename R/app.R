library(shiny)
library(r2d3)
library(shinyModules)

myApp <- function(testing = FALSE, debug = FALSE) {
  ui <- fluidPage(
    theme = "flatly.bootstrap.min.css",
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/graph.css")
    ),
    navbarPage(
      "Cluster Viewer",
      tabPanel(
        "Data upload",
        sidebarLayout(
          sidebarPanel(
            # File upload
            h3("Graph Data:"),
            uploadGraphInput("GraphData"),
            # RNASeq upload
            h3("RNA-seq Data:"),
            uploadRNASeqInput("rnaseqData")
          ),
          mainPanel(
            uploadRNASeqOutput("rnaseqData"),
            # file output for testing
            h3("Nodes:"),
            tableOutput("nodes"),
            h3("Edges:"),
            tableOutput("edges")
          )
        )
      ),
    )
  )
  server <- function(input, output, session) {
    data_list <- uploadGraphServer(id = "GraphData", debug = debug)
    output$nodes <- renderTable(data_list$nodes()[1:5,])
    output$edges <- renderTable(data_list$edges()[1:5,])

    rnaseq_data_list <- uploadRNASeqServer(id = "rnaseqData", debug = debug)
    output$samples <- renderTable(rnaseq_data_list$sample_info()[1:5,1:5])
    output$counts <- renderTable(rnaseq_data_list$counts()[1:5,1:10])
    output$metadata <- renderTable(rnaseq_data_list$gene_metadata()[1:5,])

  }
  shinyApp(ui, server)
}
