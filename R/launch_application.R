# Launch the Shiny app
#' Starts the R shiny app to define the settings and then run the HIA Java app.
#'
#' @param debug A boolean. The default is `FALSE`. If `TRUE`, the elements in
#' app show the reactive values they return. This is useful for interactively
#' debugging the app.
#'
#' @returns A graphical user interface with the Shiny app.
#' @export
#'
#' @examples
#' \dontrun{
#' launch_application()
#' }
launch_application <- function(debug = FALSE) {
  shinyOptions(debug = debug)
  runApp(appDir = system.file("application", package = "hiaR"))
}
