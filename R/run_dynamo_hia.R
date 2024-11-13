#' Run the DYNAMO-HIA simulation
#'
#' A wrapper for running the Java application. Runs the application in batch mode.
#'
#' @param simulation_filename Path to the simulation batch file. Each line of this file must contain
#' a directory name of a simulation that should be run.
#' @param app_filename Path to the Java application.
#' @param log_filename Path to a log file. Can also be `FALSE` to disable logging.
#'
#' @return Returns \code{TRUE} if the simulations ran successfully. Otherwise, it will forward the
#' exception returned by the Java application.
#' @export
#'
#' @example
#' \dontrun{
#' run_dynamo_hia("simulation_batch.txt", "DYNAMO-HIA")
#' }
run_dynamo_hia <- function(simulation_filename, app_filename, log_filename = "run_dynamo_hia.log") {
  output <- system2(
    command = app_filename,
    args = simulation_filename,
    stdout = log_filename,
    stderr = TRUE
  )

  if (length(output) > 0) {
    stop(output)
  }

  return(TRUE)
}
