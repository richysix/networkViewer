#' UI for the advancedShiny app
app_ui <- function() {
  testing <- shiny::getShinyOption("testing", default = FALSE)

  bslib::page_fluid(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/graph.css")
    ),
    navbarPage(
      theme = bslib::bs_theme(bootswatch = "cosmo"),
      inverse = TRUE,
      title = "Cluster Viewer",
      tabPanel(
        "Data upload",
        sidebarLayout(
          sidebarPanel(
            # File upload
            h3("Graph Data:"),
            uploadGraphInput("GraphData", testing = testing),
            # RNASeq upload
            h3("RNA-seq Data:"),
            shinyModules::uploadRNASeqInput("rnaseqData", testing = testing)
          ),
          mainPanel(
            # file output for testing
            verbatimTextOutput("testing_msgs"),
            h3(id = "pink", "Nodes:"),
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
          accordion(
            accordion_panel(
              title = "Network layout controls",
              networkOverviewInput("network_overview"),
            ),
            open = FALSE
          ),
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
}
