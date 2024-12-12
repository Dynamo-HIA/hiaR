#' UI part for simulation config
#'
#' @param id The id for the namespace of the module.
#' @param population_choices A character vector with possible choices of
#' populations.
#'
#' @keywords internal
#'
simulation_config_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("simulation_name"), "Simulation name:", ""),
    selectInput(ns("population"), "Population:", c("")),
    radioButtons(ns("newborns"), "Newborns:", c("yes", "no")),
    numericInput(ns("population_size"), "Simulated population size:", value = 50),
    numericInput(ns("starting_year"), "Starting year:", value = 2010),
    numericInput(ns("years"), "Number of years:", value = 25),
    numericInput(ns("time_step"), "Calculation time step:", value = 1),
    numericInput(ns("random_seed"), "Random seed:", value = 1),
    actionButton(ns("save"), "Save and Run", class = "btn-lg btn-success")
  )
}



#' Server part for simulation config
#'
#' Processes all user input data from the UI into a reactive list.
#' Uses some reference meta data for letting the user choose between options.
#' When the `save` button is clicked, it will start and run the Java app
#' (not yet implemented).
#'
#' @param id  The module namespace id, matching the `id` in the UI part.
#' @param reference_data A reactive, containing the reference meta data.
#'
#' @returns A reactive list of key-value pairs read from the config UI, to be
#' used in the app/by other modules.
#'
#' @keywords internal
#'
simulation_config_server <- function(id, reference_data) {
  moduleServer(
    id,
    function(input, output, session) {
      user_data <- reactive({
        list(
          simulation_name = input$simulation_name,
          population = input$population,
          newborns = input$newborns,
          population_size = input$population_size,
          starting_year = input$starting_year,
          years = input$years,
          time_step = input$time_step,
          random_seed = input$random_seed
        )
      })

      observeEvent( # TODO: send data to Java program from here
        input$save,
        {
          message("Running simulation now")
          # message(paste0("user_data$population is now ", user_data()$population))
          print("How much input checking to do?")
          # if (input$dir_simulation == "")
          #  showNotification("Select simulation directory first", type='warning')
        }
      )

      observeEvent(reference_data(), {
        updateSelectInput(session, "population",
          choices = names(reference_data()$population),
          selected = NULL
        )
      })

      return(user_data)
    }
  )
}
