
#' UI part for program config
#'
#' Creates two panels for the user to choose the paths to the DYNAMO executable
#' and to the working directory.
#'
#' @param id The id for the namespace of the module.
#'
#' @returns Shows an error message when the reference data is not NULL and is
#' does not have the right content.
#'
#' @export
program_config_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("validation_msg")),
    wellPanel(
      textInput(ns("working_path"), label = "Working directory:"),
      shinyFiles::shinyDirButton(
        id = ns("working_button"),
        title = "Set the path to the working directory",
        label = "Set directory",
        multiple = FALSE
      )),
    wellPanel(
      textInput(ns("dynamo_path"), label = "Dynamo executable:"),
      shinyFiles::shinyFilesButton(
        id = ns("dynamo_button"),
        title = "Set the file to the Dynamo executable",
        label = "Set file",
        multiple = FALSE
      )
    )
    )
}




#' Server part for program config
#'
#' The function observes the values in the UI, loads reference meta data and
#' sends them back as reactive values to the caller.
#'
#' @param id The module namespace id, matching the `id` in the UI part.
#'
#' @returns A named list of reactive values with the following elements
#' \itemize{
#'  \item reference_data: A list loaded with \link{get_reference_data}.
#'  \item dynamo_path: Path to the DYNAMO executable.
#'  \item working_path: Working directory for the DYNAMO program.
#' }
#' @export
program_config_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session){
      reference_data <- reactiveVal(NULL)
      working_path <- reactiveVal(NULL)
      dynamo_path <- reactiveVal(NULL)

      output$validation_msg <- renderUI({
        # Check if reference_data() is available and the diseases list is not empty
        if (!is.null(reference_data()) && length(reference_data()$diseases) == 0) {
          div(
            class = "alert alert-warning",
            "Invalid path to working directory."
          )
        } else {
          NULL
        }
      })

      observeEvent(
        input$working_button, {
          volumes <- c(
            Home = fs::path_home(),
            shinyFiles::getVolumes()()
          )

          shinyFiles::shinyDirChoose(
            input, "working_button", roots = volumes, session = session)

          if (!is.null(input$working_button)) {
            selected_path <- parse_dirpath_wrapper(volumes, input$working_button)
            if (length(selected_path) > 0) {
              updateTextInput(
                session, "working_path", value = selected_path)
              working_path(selected_path)
              new_reference_data <- get_reference_data(file.path(selected_path, "Reference_Data"))
              reference_data(new_reference_data)
            }
          }
        }
      )

      observeEvent(
        input$dynamo_button, {
          volumes <- c(
            Home = fs::path_home(),
            shinyFiles::getVolumes()()
          )

          shinyFiles::shinyFileChoose(
            input, "dynamo_button", roots = volumes, session = session
          )

          if (!is.null(input$dynamo_button)) {
            selected_path_df <- parse_filepath_wrapper(volumes, input$dynamo_button)
            n_rows <- dim(selected_path_df)[1]
            if (n_rows > 0) {
              selected_path <- selected_path_df$datapath[[1]]
              updateTextInput(
                session, "dynamo_path", value = selected_path
              )
              dynamo_path(selected_path)
            }
          }

        }
      )

      return(list(
        working_path = working_path,
        dynamo_path = dynamo_path,
        reference_data = reference_data
      ))
    }
  )

}
