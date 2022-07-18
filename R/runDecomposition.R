#'@export
runDecomposition <- function() {
  appDir <- system.file("shiny-apps", "lwr", package = "DeSeaTS")
  if (appDir == "") {
    stop('Could not find example directory. Try re-installing "DeSeaTS".', call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}