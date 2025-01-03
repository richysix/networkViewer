library(shiny)
library(r2d3)
library(shinyModules)
library(bslib)

myApp <- function(testing = FALSE, debug = FALSE) {
  ui <- tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/graph.css")
    ),
    navbarPage(
      theme = bslib::bs_theme(bootswatch = "cosmo"),
      inverse = TRUE,
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
            verbatimTextOutput("testing_msgs"),
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
        card(
          card_header("Count Plot"),
          layout_sidebar(
            sidebar = countPlotInput("gene1"),
            countPlotOutput("gene1")
          )
        ),
        card(
          card_header("XY scatterplot"),
          layout_sidebar(
            sidebar = countXYScatterPlotInput(id = "xy_scatter", hide_x = TRUE),
            countXYScatterPlotOutput("xy_scatter")
          )
        )
      ),
    )
  )
  server <- function(input, output, session) {
    if (testing) {
      output$testing_msgs <-
        renderPrint({
          glue::glue(
            "In networkViewer app server function:\n",
            "Bootstrap version is: ",
            "{bslib::theme_version(shiny::getCurrentTheme())}")
        })
    }

    data_list <- uploadGraphServer(id = "GraphData", debug = debug)
    output$nodes <- renderTable(data_list$nodes()[1:5,])
    output$edges <- renderTable(data_list$edges()[1:5,])

    rnaseq_data_list <- uploadRNASeqServer(id = "rnaseqData", testing = testing,
                                           debug = debug)
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
