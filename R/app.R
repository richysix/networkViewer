library(shiny)
library(r2d3)
library(shinyModules)
library(bslib)
library(DT)

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
        "Network Overview",
        page_fillable(
          networkOverviewOutput("network_overview"),
          layout_columns(
            card(
              card_header("Network overview (clusters)"),
              DT::dataTableOutput("cl_nodes"),
            ),
            card(
              card_header("Network overview (connections)"),
              DT::dataTableOutput("cl_edges")
            )
          )
        )
      ),
      tabPanel(
        "Cluster View",
        page_fillable(
          subsetGraphInput("cluster_closeup"),
          subsetGraphOutput("cluster_closeup")
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
        ),
        card(
          card_header("Count values"),
          DTOutput("count_data"),
        ),
      ),
    )
  )
  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 50*1024^2)
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

    # Overview
    overview_list <- networkOverviewServer(
      "network_overview",
      nodes = data_list$nodes,
      edges = data_list$edges,
      debug
    )
    output$cl_nodes <- DT::renderDataTable(overview_list$overview_nodes())
    output$cl_edges <- DT::renderDataTable(overview_list$overview_edges())

    # Cluster View
    subset_list <- subsetGraphServer(
      id = "cluster_closeup",
      nodes = data_list$nodes,
      edges = data_list$edges,
      debug = debug
    )

    # TODO: remove genes with all zeros
    rnaseq_data_list <- uploadRNASeqServer(id = "rnaseqData", testing = testing,
                                           debug = debug)

    output$samples <- renderTable({
      req(rnaseq_data_list$sample_info())
      samples <- rnaseq_data_list$sample_info()
      max_width <- 5
      n_cols <- ifelse(ncol(samples) > max_width, max_width, ncol(samples))
      samples[1:5,seq_len(n_cols)]
    })
    output$counts <- renderTable({
      req(rnaseq_data_list$counts())
      counts <- rnaseq_data_list$counts()
      max_width <- 10
      n_cols <- ifelse(ncol(counts) > max_width, max_width, ncol(counts))
      counts[1:5,seq_len(n_cols)]
    })
    output$metadata <- renderTable({
      req(rnaseq_data_list$gene_metadata())
      gene_metadata <- rnaseq_data_list$gene_metadata()
      max_width <- 5
      n_cols <- ifelse(ncol(gene_metadata) > max_width, max_width, ncol(gene_metadata))
      gene_metadata[1:5,seq_len(n_cols)]
    })

    selected_gene <- countPlotServer(
      id = "gene1",
      counts = rnaseq_data_list$counts,
      sample_info = rnaseq_data_list$sample_info,
      gene_metadata = rnaseq_data_list$gene_metadata,
      debug = debug
    )

    plot_data_list <- countXYScatterPlotServer(
      "xy_scatter",
      counts = rnaseq_data_list$counts,
      sample_info = rnaseq_data_list$sample_info,
      gene_metadata = rnaseq_data_list$gene_metadata,
      gene1 = selected_gene$gene1,
      debug = debug
    )

    output$count_data <- renderDT(
      plot_data_list$plot_data(),
      options = list(pageLength = nrow(plot_data_list$plot_data()))
    )

  }
  shinyApp(ui, server)
}
