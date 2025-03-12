#' Create UI components to display a graph
#'
#' `networkOverviewInput()` produces controls to alter how the visualisation works
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [networkOverviewServer()] function.
#'
#' @returns a [htmltools::tagList()] containing a [shiny::sliderInput()] and
#' a [shiny::actionButton()] to update the parameters
#' any changes only take effect once the button has been clicked so that the
#' simulation is not constantly recalculating
#'
#' @export
#'
#' @examples
#'
#' networkOverviewInput("graph")
#'
networkOverviewInput <- function(id, ...) {
  tagList(
    graphD3Input(NS(id, "network_overview_graph"), ...)
  )
}

#' Create output components to display graph subset
#'
#' `networkOverviewOutput()` produces an output for a graphD3Output
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [networkOverviewServer()] function.
#'
#' @returns a [htmltools::tagList()] containing a [networkD3Output()]
#'
#' @export
#'
#' @examples
#'
#' networkOverviewOutput("graph")
#'
networkOverviewOutput <- function(id) {
  tagList(
    graphD3Output(NS(id, "network_overview_graph"))
  )
}

#' Server function to upload sample and count data files
#'
#' `networkOverviewServer()` converts the network to an overview where the nodes
#' represent clusters in the network, where the nodes are sized according the
#' number of nodes in the cluster and the edges are size according to the number
#' of edges from one cluster to another
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [networkOverviewInput()] function.
#'
#' @returns a list containing two [shiny::reactive()] objects
#' * overview_nodes a data.frame of the nodes representing clusters
#' * overview_edges a data.frame of the edges summarising the connections between clusters
#'
#' @export
#'
#' @examples
#'
#' networkOverviewServer("GraphData")
#'
networkOverviewServer <- function(id, nodes = NULL, edges = NULL, debug = FALSE) {
  stopifnot(is.reactive(nodes))
  stopifnot(is.reactive(edges))

  moduleServer(id, function(input, output, session) {
    # create overview nodes data.frame from nodes
    overview_nodes <- reactive({
      req(nodes())
      if (debug) {
        cat("networkOverviewServer: input nodes\n")
        print(head(nodes()))
      }
      create_overview_nodes(nodes())
    })

    # subset edges to cluster
    overview_edges <- reactive({
      req(nodes(), edges())
      if (debug) {
        cat("networkOverviewServer: input edges\n")
        print(head(edges()))
      }
      create_overview_edges(nodes(), edges())
    })

    # draw network
    graphD3Server(
      id = "network_overview_graph",
      nodes = overview_nodes,
      edges = overview_edges,
      d3_options = list(
        "use_size" = TRUE,
        "use_weight_as_stroke" = TRUE,
        "scale_weights" = TRUE,
        "stroke_scale_factor" = 10,
        "colour_nodes" = TRUE
      ),
      debug = debug
    )

    # return the subset of nodes and edges
    list(
      overview_nodes = overview_nodes,
      overview_edges = overview_edges
    )
  })
}

#' Create an overview of the nodes of a network
#'
#' `create_overview_nodes()` produces a data.frame summarising the nodes in a
#' network by cluster
#'
#' @param nodes network nodes represented as a data.frame
#'
#' @returns data.frame with columns
#' node_idx
#' node_size
#' cluster_id
#'
#' @export
#'
#' @examples
#'
#' nodes <- readr::read_csv(system.file("extdata/nodes-test-mini.csv", package = "networkViewer"))
#' create_overview_nodes(nodes)
#'
create_overview_nodes <- function(nodes) {
  cluster_ids <- unique(nodes$cluster_id)
  data.frame(
    node_idx = cluster_ids,
    size = purrr::map_int(split(nodes, nodes$cluster_id),
                          \(x) nrow(x) ),
    cluster_id = cluster_ids
  )
}

#' Create an overview of the edges of a network
#'
#' `create_overview_nodes()` produces a data.frame summarising the edges in a
#' network by cluster
#'
#' @param nodes network nodes represented as a data.frame
#' @param edges network edges represented as a data.frame
#'
#' @returns data.frame with columns
#' edge_idx
#' source
#' target
#' weight
#'
#' @export
#'
#' @examples
#'
#' edges <- readr::read_csv(system.file("extdata/edges-test-mini.csv", package = "networkViewer"))
#' create_overview_nodes(edges)
#'
create_overview_edges <- function(nodes, edges) {
  cluster_lookup <- nodes$cluster_id
  names(cluster_lookup) <- nodes$node_idx

  edges$source_cluster <- unname(cluster_lookup[ as.character(edges$source) ])
  edges$target_cluster <- unname(cluster_lookup[ as.character(edges$target) ])
  dplyr::filter(edges, source_cluster != target_cluster) |>
    dplyr::group_by(source_cluster, target_cluster) |>
    dplyr::summarise(weight = length(source), .groups = "drop") |>
    dplyr::mutate(source = dplyr::case_when(
      source_cluster < target_cluster ~ source_cluster,
      TRUE ~ target_cluster
    ), target = dplyr::case_when(
      source_cluster < target_cluster ~ target_cluster,
      TRUE ~ source_cluster
    )) |>
    dplyr::group_by(source, target) |>
    dplyr::summarise(weight = sum(weight), .groups = "drop")
}

# Function to count the edges from one cluster to another
count_edges <- function(cluster_id1, cluster_id2, nodes, edges) {
  nodes_in_cl1 <- nodes$node_idx[ nodes$cluster_id == cluster_id1 ]
  nodes_in_cl2 <- nodes$node_idx[ nodes$cluster_id == cluster_id2 ]
  from_cl1 <- sum(edges$source %in% nodes_in_cl1 & edges$target %in% nodes_in_cl2)
  from_cl2 <- sum(edges$source %in% nodes_in_cl2 & edges$target %in% nodes_in_cl1)
  from_cl1 + from_cl2
}

#' A test shiny app for the networkOverview module
#'
#' `networkOverviewApp()` creates a small test app for testing the [networkOverviewInput()],
#' [networkOverviewOutput()] and [networkOverviewServer()] functions.
#'
#' @return a [shiny::shinyApp()] object
#'
#' @examples
#' networkOverviewApp()
networkOverviewApp <- function(debug = TRUE) {
  ui <- fluidPage(
      networkOverviewInput(
        "overview",
        strength_params = list(
          width = "60%",
          min = -100, max = 0, step = 1, value = -50
        )),
      networkOverviewOutput("overview"),
      DT::dataTableOutput("nodes"),
      DT::dataTableOutput("edges")
  )
  server <- function(input, output, session) {
    node_data <- readr::read_csv(file.path(here::here(), "extdata", "nodes-test.csv"))
    edge_data <- readr::read_csv(file.path(here::here(), "extdata", "edges-test.csv"))
    data_list <- networkOverviewServer(
      "overview",
      nodes = reactive(node_data),
      edges = reactive(edge_data),
      debug
    )

    output$nodes <- DT::renderDataTable(data_list$overview_nodes())
    output$edges <- DT::renderDataTable(data_list$overview_edges())
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
