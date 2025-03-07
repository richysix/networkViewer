#' Create UI components to upload node and edges files for a graph
#'
#' `graphD3Output()` produces a d3 network visualisation of input data
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [graphD3Server()] function.
#'
#' @returns a [htmltools::tagList()] containing a [r2d3::d3Output()]
#'
#' @export
#'
#' @examples
#'
#' graphD3Output("graph")
#'
graphD3Output <- function(id, svg_height = 800) {
  tagList(
    d3Output(NS(id, "d3_graph"), height = svg_height)
  )
}

#' Server function to upload sample and count data files
#'
#' `graphD3Server()` implements uploading a sample file and a count data
#' file. It also handles using the package test data.
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [graphD3Output()] function.
#'
#' @returns a list containing two [shiny::reactive()] objects
#' * sampleInfo a data.frame of sample metadata
#' * counts a data.frame of RNAseq count data
#'
#' @export
#'
#' @examples
#'
#' graphD3Server("GraphData")
#'
graphD3Server <- function(id, nodes = NULL, edges = NULL, d3_options = NULL, debug = FALSE) {
  stopifnot(is.reactive(nodes))
  stopifnot(is.reactive(edges))
  stopifnot(!is.reactive(d3_options))

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
        d3_version = "6",
        options = d3_options,
        # this does not work
        sizing = htmlwidgets::sizingPolicy(
          # padding = 0, browser.fill = TRUE,
          defaultHeight = "800px"
        )
      )
    })
  })
}

#' A test shiny app for the network module
#'
#' `graphD3App()` creates a small test app for testing the [graphD3Output()] and
#' [graphD3Server()] functions.
#'
#' @return a [shiny::shinyApp()] object
#'
#' @examples
#' graphD3App()
graphD3App <- function(debug = TRUE, use_size = FALSE, use_weight = FALSE,
                       scale_weights = FALSE, colour_nodes = FALSE) {
  ui <- fluidPage(
        graphD3Output("graph")
  )
  server <- function(input, output, session) {
    node_data <- readr::read_rds(file.path(here::here(), "tests", "testthat",
                                    "fixtures", "nodes-mini.rds"))
    edge_data <- readr::read_rds(file.path(here::here(), "tests", "testthat",
                                    "fixtures", "edges-mini.rds"))
    graphD3Server(
      "graph",
      nodes = reactive(node_data),
      edges = reactive(edge_data),
      d3_options = list("use_size" = use_size,
                        "use_weight" = use_weight,
                        "scale_weights" = scale_weights,
                        "colour_nodes" = colour_nodes),
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
# This software is Copyright (c) 2024 Queen Mary University of London.
#
# This is free software, licensed under:
#
#  The GNU General Public License, Version 3, June 2007
