#' Create UI components to upload node and edges files for a graph
#'
#' `uploadGraphInput()` produces the buttons for uploading node and edge data
#' files. It also provides a checkbox to use the package test data. If node and
#' edge files are subsequently uploaded, this is used instead of the test data.
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [uploadGraphServer()] function.
#'
#' @returns a [htmltools::tagList()] containing two [shiny::fileInput()] controls and
#' a [shiny::checkboxInput()].
#'
#' @export
#'
#' @examples
#'
#' uploadGraphInput("GraphData")
#'
uploadGraphInput <- function(id) {
  tagList(
    fileInput(NS(id, "nodesFile"), "Nodes File"),
    fileInput(NS(id, "edgesFile"), "Edges File"),
    checkboxInput(NS(id, "testdata"), 'Use test data', value = FALSE, width = NULL)
  )
}

#' Server function to upload sample and count data files
#'
#' `uploadGraphServer()` implements uploading a sample file and a count data
#' file. It also handles using the package test data.
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [uploadGraphInput()] function.
#'
#' @returns a list containing two [shiny::reactive()] objects
#' * sampleInfo a data.frame of sample metadata
#' * counts a data.frame of RNAseq count data
#'
#' @export
#'
#' @examples
#'
#' uploadGraphServer("GraphData")
#'
uploadGraphServer <- function(id, debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    # set up observer to untick test data checkbox if a node/edge
    # file is uploaded
    observe({
      updateCheckboxInput(session, "testdata", value = FALSE)
    }) |>
      bindEvent(input$nodesFile, input$edgesFile)

    # return nodes file path depending on whether the test data checkbox is checked
    nodes_file <- reactive({
      if (input$testdata) {
        #file_path <- system.file("extdata", "nodes-test-mini.csv", package = "networkViewer")
        file_path <- file.path("", "Users", "rjw26", "checkouts", "networkViewer", "extdata", "nodes-test-mini.csv")
        if (debug) message(paste0("Nodes file path = ", file_path))
        return(file_path)
      } else {
        if (debug) message(paste0("Nodes file path = ", input$nodesFile$name))
        return(input$nodesFile$datapath)
      }
    })

    # load node data from file
    init_nodes <- reactive({
      req(nodes_file())
      load_node_data(nodes_file())
    })

    # return edges file path depending on whether the test data checkbox is checked
    edges_file <- reactive({
      if (input$testdata) {
        # file_path <- system.file("extdata", "edges-test-mini.csv", package = "network-viewer")
        file_path <- file.path("", "Users", "rjw26", "checkouts", "networkViewer", "extdata", "edges-test-mini.csv")
        if (debug) message(paste0("Edges file path = ", file_path))
        return(file_path)
      } else {
        if (debug) message(paste0("Edges file path = ", input$edgesFile$name))
        return(input$edgesFile$datapath)
      }
    })
    # load edges data from file
    init_edges <- reactive({
      req(edges_file())
      edges <- load_edge_data(edges_file())
      if (debug) {
        message("Loaded edges file:")
        print(head(edges))
      }
      return(edges)
    })

    # return the nodes and edges
    list(
      nodes = init_nodes,
      edges = init_edges
    )
  })
}

#' Load Node data
#'
#' \code{load_node_data} loads the nodes file and checks it contains the
#' minimum required columns (node_idx)
#'
#' @param data_file Name of file to load
#' @param ... Other arguments passed on to the [readr] functions
#'
#' @return tibble The loaded data
#'
load_node_data <- function(file_path, ...) {
  node_df <- load_data(file_path, ...)
  if (!("node_idx" %in% colnames(node_df))) {
    rlang::abort("No column 'node_idx'! This is a required column.",
                 class = "error_missing_column")
  }
  return(node_df)
}

#' Load Edge data
#'
#' \code{load_edge_data} loads the edges file and checks it contains the
#' minimum required columns (source, target)
#'
#' @param data_file Name of file to load
#' @param ... Other arguments passed on to the [readr] functions
#'
#' @return tibble The loaded data
#'
load_edge_data <- function(file_path, ...) {
  edge_df <- load_data(file_path, ...)
  required_cols <- c("source", "target")
  if (any(!required_cols %in% colnames(edge_df))) {
    missing_cols <- c("source", "target")[!required_cols %in% colnames(edge_df)]
    rlang::abort(paste("Missing columns:", paste0(missing_cols, collapse = ", ")),
                 class = "error_missing_column")
  }
  return(edge_df)
}

#' Load data
#'
#' \code{load_data} is the underlying function used to load data
#' It automatically detects the delimiter based on the file name and uses the
#' appropriate [readr] function.
#' Possibilities are [readr::read_csv()], [readr::read_tsv()] or [readr::readr_delim()]
#' [readr::read_delim()] will require the delimiter passing in as an argument to
#' [load_node_data()] or [load_edge_data()]
#'
#' @param data_file Name of file to load
#' @param ... Other arguments passed on to the [readr] functions
#'
#' @return tibble The loaded data
#'
load_data <- function(data_file, ...) {
  if (sub("\\.gz$", "", data_file) |> (\(x) grepl("\\.tsv", x))()) {
    readr_func <- readr::read_tsv
  } else if (sub("\\.gz$", "", data_file) |> (\(x) grepl("\\.csv", x))()) {
    readr_func <- readr::read_csv
  } else {
    readr_func <- readr::read_delim
  }

  coltypes <- set_col_types(data_file, readr_func, ...)

  # Read data
  data <- readr_func(data_file, col_types = coltypes, ...)

  return(data)
}

#' Set the column types based on the column names
#'
#' \code{set_col_types} loads the first line of
#' the data and sets the column type based on the column names.
#' Chr and Strand are factor and Start and End are integer.
#' normalised count columns are double (float) and
#' count columns are integer.
#' All other columns are set to the type guessed by read_tsv
#'
#' @param data_file Name of file to open
#'
#' @return a readr::cols specification
#'
#' @examples
#' data <- standardise_colnames('test_data.tsv')
#'
set_col_types <- function(data_file, readr_func, ...){
  types_for_cols = c(
    "node_idx" = "i",
    "node_id" = "i",
    "id" = "i",
    "gene_id" = "c",
    "gene_name" = "c",
    "description" = "c",
    "cluster_id" = "i",
    "singleton" = "l",
    "edge_idx" = "i",
    "edge_id" = "i",
    "source" = "i",
    "target" = "i",
    "weight" = "d"
  )

  # get columns
  header <- readr_func(data_file, n_max = 1000,
                       col_types = readr::cols(), ...)
  # standard_colnames <- colnames(standardise_colnames(header))

  coltype_for_column_name <- function(colname, types_for_cols) {
    if (colname %in% names(types_for_cols)) {
      if (types_for_cols[[colname]] == "f") {
        return(readr::col_factor())
      } else if (types_for_cols[[colname]] == "i") {
        return(readr::col_integer())
      } else if (types_for_cols[[colname]] == "c") {
        return(readr::col_character())
      } else if (types_for_cols[[colname]] == "d") {
        return(readr::col_double())
      }
    } else {
      return(readr::spec(header)$cols[[colname]])
    }
  }
  # set column types
  column_types <-
    purrr::map(colnames(header),
                \(x) coltype_for_column_name(x, types_for_cols))
  names(column_types) <- colnames(header)
  return(do.call(readr::cols, column_types))
}

#' A test shiny app for the uploadGraph module
#'
#' `uploadGraphApp()` creates a small test app for testing the [uploadGraphInput()] and
#' [uploadGraphServer()] functions. A subset of the returned node and edge data.frames
#' are displayed in two [shiny::tableOutput()]s
#'
#' @return a [shiny::shinyApp()] object
#'
#' @examples
#' uploadGraphApp()
uploadGraphApp <- function(debug = FALSE) {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        uploadGraphInput("GraphData")
      ),
      mainPanel(
        tableOutput("nodes"),
        tableOutput("edges")
      )
    )
  )
  server <- function(input, output, session) {
    data_list <- uploadGraphServer("GraphData", debug)
    output$nodes <- renderTable(data_list$nodes()[1:5,])
    output$edges <- renderTable(data_list$edges()[1:5,])
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


#' #' Create Output to display uploaded sample and count data
#' #'
#' #' `uploadRNASeqOutput()` produces three table outputs for displaying
#' #' the data returned by the `uploadRNASeqServer()` function. The three
#' #' outputs are labelled samples, counts and metadata. There are also two
#' #' [shinyBS::bsAlert()] anchor points to alert the user to missing data in
#' #' either the sample or count file
#' #'
#' #' @param id namespace id for the UI components. Must match the id provided to the
#' #' [uploadRNASeqServer()] function.
#' #'
#' #' @returns a [htmltools::tagList()] containing three [shiny::tableOutput()]s
#' #' and two [shinyBS::bsAlert()] anchor points.
#' #'
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' uploadRNASeqOutput("rnaseqData")
#' #'
#' uploadRNASeqOutput <- function(id) {
#'   tagList(
#'     h3("Sample Data:"),
#'     shinyBS::bsAlert(NS(id, "countsInputAlert")),
#'     tableOutput("samples"),
#'     h3("Count Data:"),
#'     shinyBS::bsAlert(NS(id, "sampleInputAlert")),
#'     tableOutput("counts"),
#'     h3("Gene Metadata:"),
#'     tableOutput("metadata")
#'   )
#' }
