#' Run the DYNAMO-HIA simulation
#'
#' A wrapper for running the Java application. Runs the application in batch mode.
#'
#' @param simulation_filename Path to the simulation batch file. Each line of this file must contain
#' a directory name of a simulation that should be run. This file should be located in the
#' "working directory", i.e., a directory that contains the `Reference_Data` and `Simulations`
#' subdirectories.
#' @param app_filename Path to the Java application.
#' @param log_filename Path to a log file. The default filename is `run_dynamo_hia.log`.
#'  Can also be `FALSE` to disable logging.
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
  withr::with_tempfile("std_err", {
    sys::exec_wait(
      cmd = app_filename,
      args = simulation_filename,
      std_out = log_filename,
      std_err = std_err
    )

    errors <- readLines(std_err)

    if (length(errors) > 0) {
      stop(errors)
    }
  })

  return(TRUE)
}
