#' Run the app
#'
#' Initiate an instance of the networkViewwr app. If a path to a directory is
#' supplied, use that. Otherwise, if the current context is interactive,
#' (i.e. if the function is called from RStudio), this function will initiate the app stored in
#' inst/app with the runApp function. If the context is not interactive (i.e. the project is being sourced in RStudio
#' Connect after having been deployed), then the function will return a shiny.appobj object, using the function
#' shinyAppDir. This distinction is necessary to allow the app to be deployed.
#'#'
#' @return either run the app as a side effect or return a shiny.appobj object
#'
#' @importFrom shiny runApp shinyAppDir
#'
#' @export
run_netView_app <- function(dir = NULL) {

  if (!is.null(dir)) {

    runApp(appDir = dir)

  } else if (interactive()) {

    runApp(appDir = system.file("net_view_app",
                                package = "networkViewer"))

  } else {

    shinyAppDir(appDir = system.file("net_view_app",
                                     package = "networkViewer"))

  }

}
