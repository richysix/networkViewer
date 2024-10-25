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
            # file output for testing
            h3("Nodes:"),
            tableOutput("nodes"),
            h3("Edges:"),
            tableOutput("edges"),
            uploadRNASeqOutput("rnaseqData"),
          )
        )
      ),
      tabPanel(
        "Gene Data",
        sidebarLayout(
          sidebarPanel(
            countPlotInput("gene1"),
            hr(),
            countXYScatterPlotInput("xy_scatter")
          ),
          mainPanel(
            h3("Count Plot"),
            countPlotOutput("gene1"),
            h3("XY scatterplot"),
            countXYScatterPlotOutput("xy_scatter")
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

    selected_gene <- countPlotServer(
      id = "gene1",
      counts = rnaseq_data_list$counts,
      sample_info = rnaseq_data_list$sample_info,
      gene_metadata = rnaseq_data_list$gene_metadata,
      debug = debug
    )

    countXYScatterPlotServer(
      "xy_scatter",
      counts = rnaseq_data_list$counts,
      sample_info = rnaseq_data_list$sample_info,
      gene_metadata = rnaseq_data_list$gene_metadata,
      gene1 = selected_gene$gene1,
      debug = debug
    )
  }
  shinyApp(ui, server)
}
