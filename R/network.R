#' Create UI components to upload node and edges files for a graph
#'
#' `networkD3Output()` produces a d3 network visualisation of input data
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [networkD3Server()] function.
#'
#' @returns a [htmltools::tagList()] containing a [r2d3::d3Output()]
#'
#' @export
#'
#' @examples
#'
#' networkD3Output("graph")
#'
networkD3Output <- function(id) {
  tagList(
    d3Output(NS(id, "d3_graph"))
  )
}

#' Server function to upload sample and count data files
#'
#' `networkD3Server()` implements uploading a sample file and a count data
#' file. It also handles using the package test data.
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [networkD3Output()] function.
#'
#' @returns a list containing two [shiny::reactive()] objects
#' * sampleInfo a data.frame of sample metadata
#' * counts a data.frame of RNAseq count data
#'
#' @export
#'
#' @examples
#'
#' networkD3Server("GraphData")
#'
networkD3Server <- function(id, nodes = NULL, edges = NULL, debug = FALSE) {
  stopifnot(is.reactive(nodes))
  stopifnot(is.reactive(edges))

  moduleServer(id, function(input, output, session) {
    output$d3_graph <- renderD3({
      req(nodes())
      req(edges())
      if (debug) {
        print(head(nodes()))
        print(head(edges()))
      }

      r2d3(
        data = list("nodes" = jsonlite::toJSON(nodes()),
                    "edges" = jsonlite::toJSON(edges())),
        script = file.path(here::here(), "www", "js", "graph-svg.js"),
        css = file.path(here::here(), "www", "css", "graph.css"),
        d3_version = "6"
      )
    })
  })
}

#' A test shiny app for the network module
#'
#' `networkD3App()` creates a small test app for testing the [networkD3Output()] and
#' [networkD3Server()] functions.
#'
#' @return a [shiny::shinyApp()] object
#'
#' @examples
#' networkD3App()
networkD3App <- function(debug = FALSE) {
  ui <- fluidPage(
        networkD3Output("graph")
  )
  server <- function(input, output, session) {
    node_data <- read_rds(file.path(here::here(), "tests", "testthat",
                                    "fixtures", "nodes-mini.rds"))
    edge_data <- read_rds(file.path(here::here(), "tests", "testthat",
                                    "fixtures", "edges-mini.rds"))
    networkD3Server("graph", nodes = reactive(node_data),
                  edges = reactive(edge_data), debug)
  }
  shinyApp(ui, server)
}

# AUTHOR
#
# Richard White <rich@buschlab.org>
#
# COPYRIGHT AND LICENSE
#
# This software is Copyright (c) 2024 Queen Mary University of London.
#
# This is free software, licensed under:
#
#  The GNU General Public License, Version 3, June 2007
