#'@export
runDecomposition <- function() {
  appDir <- system.file("shiny-apps", "lwr", package = "deseats")
  if (appDir == "") {
    stop('Could not find example directory. Try re-installing "deseats".', call. = FALSE)
  }

  .f1 <- zoo::zoo
  .f2 <- tools::vignetteInfo
  shiny::runApp(appDir, display.mode = "normal")
}