#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny runApp
run_app <- function() {
  shiny::runApp(system.file("app", package = "bddwc.app"), launch.browser = TRUE, port = 875)
}
