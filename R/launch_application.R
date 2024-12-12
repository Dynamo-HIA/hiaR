# Launch the Shiny app
#' Starts the R shiny app to define the settings and then run the HIA Java app.
#'
#' @return A graphical user interface with the Shiny app.
#' @export
#'
#' @examples
#' \dontrun{
#' launch_application()
#' }
launch_application <- function() {
  runApp(appDir = system.file("application", package = "hiaR"))
}
