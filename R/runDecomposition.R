#'@export
runDecomposition <- function() {
  appDir <- system.file("shiny-apps", "lwr", package = "deseats")
  if (appDir == "") {
    stop('Could not find example directory. Try re-installing "deseats".', call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}