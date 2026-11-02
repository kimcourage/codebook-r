# Construct without starting an HTTP server (also used by installation tests).
create_app <- function() {
  shiny::shinyApp(ui = codebookr_ui(), server = codebookr_server,
                 onStart = function() {
                   old <- options(shiny.maxRequestSize = 100 * 1024^2)
                   shiny::onStop(function() options(old))
                 })
}

#' Launch the CodebookR application
#' @param launch.browser Whether to open the application in a browser.
#' @param host Interface on which to listen. Defaults to the local computer.
#' @param port Port to use, or NULL for Shiny's default selection.
#' @return The value returned by shiny::runApp(), invisibly.
#' @export
run_app <- function(launch.browser = TRUE, host = "127.0.0.1", port = NULL) {
  shiny::runApp(create_app(), launch.browser = launch.browser, host = host, port = port)
}

example_path <- function(name) {
  system.file("examples", name, package = "codebookr", mustWork = TRUE)
}
