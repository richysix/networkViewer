#' Create UI components to select cluster to display
#'
#' `subsetGraphInput()` produces a dropdown to select a single cluster to view
#' as a graph
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [subsetGraphServer()] function.
#'
#' @returns a [htmltools::tagList()] containing a [shiny::selectInput()] control
#'
#' @export
#'
#' @examples
#'
#' subsetGraphInput("GraphData")
#'
subsetGraphInput <- function(id) {
  tagList(
    selectInput(NS(id, "cluster_num_select"), "Cluster: ", choices = NULL)
  )
}

#' Create output components to display graph subset
#'
#' `subsetGraphOutput()` produces a output for a graphD3Output
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [subsetGraphServer()] function.
#'
#' @returns a [htmltools::tagList()] containing a [graphD3Output()]
#'
#' @export
#'
#' @examples
#'
#' subsetGraphOutput("graph")
#'
subsetGraphOutput <- function(id) {
  tagList(
    graphD3Output(NS(id, "cluster_subset_graph"))
  )
}

#' Server function to upload sample and count data files
#'
#' `subsetGraphServer()` subsets the graph data to a single cluster and then
#' displays it as a graph
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [subsetGraphInput()] function.
#'
#' @returns a list containing two [shiny::reactive()] objects
#' * nodes_subset a data.frame of nodes
#' * edges_subset a data.frame of edges
#'
#' @export
#'
#' @examples
#'
#' subsetGraphServer("GraphData")
#'
subsetGraphServer <- function(id, nodes = NULL, edges = NULL, debug = FALSE) {
  stopifnot(is.reactive(nodes))
  stopifnot(is.reactive(edges))

  moduleServer(id, function(input, output, session) {
    # update cluster_num select with clusters in data
    observe({
      req(nodes())
      clusters <- unique(nodes()$cluster_id)
      updateSelectInput(
        session,
        inputId = "cluster_num_select",
        choices = clusters,
        selected = clusters[1]
      )
    }) |>
      bindEvent(nodes())

    # subset nodes to cluster
    nodes_subset <- reactive({
      req(nodes(), input$cluster_num_select)
      nodes()[ nodes()$cluster_id == input$cluster_num_select, ]
    })

    # subset edges to cluster
    edges_subset <- reactive({
      req(nodes(), edges(), input$cluster_num_select)
      nodes_in_cluster <- nodes()$node_idx[ nodes()$cluster_id == input$cluster_num_select ]
      to_keep <- edges()$source %in% nodes_in_cluster &
        edges()$target %in% nodes_in_cluster
      edges()[ to_keep, ]
    })

    # draw network
    graphD3Server(
      "cluster_subset_graph",
      nodes = nodes_subset,
      edges = edges_subset,
      d3_options = list(
        "use_size" = FALSE,
        "use_weight" = TRUE,
        "scale_weights" = FALSE,
        "colour_nodes" = TRUE
      ),
      debug = debug
    )

    # return the subset of nodes and edges
    list(
      nodes_subset = nodes_subset,
      edges_subset = edges_subset
    )
  })
}

#' A test shiny app for the subsetGraph module
#'
#' `subsetGraphApp()` creates a small test app for testing the [subsetGraphInput()],
#' [subsetGraphOutput()] and [subsetGraphServer()] functions.
#'
#' @return a [shiny::shinyApp()] object
#'
#' @examples
#' subsetGraphApp()
subsetGraphApp <- function(debug = TRUE) {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        subsetGraphInput("cluster_subset")
      ),
      mainPanel(
        subsetGraphOutput("cluster_subset"),
      )
    )
  )
  server <- function(input, output, session) {
    node_data <- readr::read_rds(file.path(here::here(), "tests", "testthat",
                                    "fixtures", "nodes-mini.rds"))
    edge_data <- readr::read_rds(file.path(here::here(), "tests", "testthat",
                                    "fixtures", "edges-mini.rds"))
    data_list <- subsetGraphServer(
      "cluster_subset",
      nodes = reactive(node_data),
      edges = reactive(edge_data),
      debug
    )
  }
  shinyApp(ui, server)
}

# AUTHOR
#
# Richard White <rich@buschlab.org>
#
# COPYRIGHT AND LICENSE
#
# This software is Copyright (c) 2025 Queen Mary University of London.
#
# This is free software, licensed under:
#
#  The GNU General Public License, Version 3, June 2007
