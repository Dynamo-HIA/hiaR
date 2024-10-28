#' Run the DYNAMO-HIA simulation
#'
#' A wrapper for the Java application. Runs the application in batch mode.
#'
#' @param simulation_filename Path to the simulation batch file.
#' Must be relative to \code{app_dirname}.
#' @param app_dirname Path to the directory of the Java application.
#' @param app_filename Name of Java application in \code{app_dirname}.
#' @param init_memory Initial allocated memory.
#' Can be \code{NULL} for default of 256M.
#' @param max_memory Maximum allocated memory.
#' Choosing more memory than the system provides will result in an error.
#'
#' @return Returns \code{TRUE} if the simulations ran successfully.
#' @export
#'
#' @example TODO
run_dynamo_hia <- function(simulation_filename,
                           app_dirname,
                           app_filename = "CZM_Mono_2.0.8.jar",
                           init_memory = NULL,
                           max_memory = "256M") {
  # TODO: It seems like there is no schema validation in Java, should this be done here?
  app_command <- "java"

  command_args <- c(
    paste0("-Xmx", max_memory),
    "-jar",
    app_filename,
    simulation_filename
  )

  if (!is.null(init_memory)) {
    command_args <- c(paste0("-Xms", init_memory), command_args)
  }

  suppressWarnings({
    output <- withr::with_dir(
      app_dirname,
      system2(
        command = app_command,
        args = command_args,
        stdout = TRUE,
        stderr = TRUE
      )
    )
  })

  status <- attr(output, "status")

  if (!is.null(status)) {
    stop(output)
  }

  # TODO: Explicitly throw error in Java program when `simulation_filename` does not exist?
  for (msg in grep("WARN", output, value = TRUE)) {
    warning(gsub(".*<(.*)>.*", "\\1", msg))
  }

  return(TRUE)
}
