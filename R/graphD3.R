#' Create UI components to display a graph
#'
#' `graphD3Input()` produces controls to alter how the visualisation works
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [graphD3Server()] function.
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
#' graphD3Input("graph")
#'
graphD3Input <- function(id, strength_params = list(), distance_params = list()) {
  strength_defaults <- list(width = "50%", min = -100, max = 100,
                            step = 20, value = -100)
  strength_defaults <- modifyList(strength_defaults, strength_params)
  distance_defaults <- list(width = "50%", min = 10, max = 200,
                            step = 10, value = 100)
  distance_defaults <- modifyList(distance_defaults, distance_params)
  tagList(
    shiny::sliderInput(
      inputId = NS(id, "charge_strength"),
      label = "Charge Strength",
      width = strength_defaults$width,
      min = strength_defaults$min, max = strength_defaults$max,
      step = strength_defaults$step, value = strength_defaults$value,
    ),
    shiny::sliderInput(
      inputId = NS(id, "link_distance"),
      label = "Link Distance",
      width = distance_defaults$width,
      min = distance_defaults$min, max = distance_defaults$max,
      step = distance_defaults$step, value = distance_defaults$value,
    ),
    shiny::actionButton(
      inputId = NS(id, "update_params"),
      label = "Update simulation parameters"
    )
  )
}

#' Create UI components to display a graph
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
    sim_params <- reactive({
      if (input$update_params > 0) {
        params <- d3_options
        params$charge_strength <- isolate(input$charge_strength)
        params$link_distance <- isolate(input$link_distance)
      } else {
        params <- d3_options
      }
      params$debug <- debug
      return(params)
    }) |> bindEvent(input$update_params, ignoreNULL = FALSE)

    output$d3_graph <- renderD3({
      req(nodes(), edges(), sim_params())
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
        options = sim_params(),
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
graphD3App <- function(debug = TRUE, use_size = FALSE, use_weight_as_stroke = FALSE,
                       scale_weights = FALSE, stroke_scale_factor = 10,
                       colour_nodes = FALSE) {
  ui <- fluidPage(
    graphD3Input("graph", strength_params = list(width = "100%"),
                 distance_params = list(value = 150)),
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
                        "use_weight_as_stroke" = use_weight_as_stroke,
                        "scale_weights" = scale_weights,
                        "colour_nodes" = colour_nodes,
                        "stroke_scale_factor" = stroke_scale_factor),
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
