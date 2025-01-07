#' UI part for simulation config
#'
#' @param id The id for the namespace of the module.
#' @param population_choices A character vector with possible choices of
#' populations.
#'
#' @export
#' @keywords internal
#'
simulation_config_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wrap_tooltip(
     textInput(ns("simulation_name"), "Simulation name:", ""),
     "The name of the simulation run."
    ),
    wrap_tooltip(
      selectInput(ns("population"), "Population:", c("")),
      "Select the population to be simulated.",
      placement = "right"
    ),
    wrap_tooltip(
      radioButtons(ns("newborns"), "Newborns:", c("yes", "no")),
      "Should newborns be included in the simulation? Doing so is only possible
      when the minimum age of the scenario is set to zero."
    ),
    wrap_tooltip(
      numericInput(ns("population_size"), "Simulated population size:", value = 50),
      "The number of persons to simulate. The number entered here is the number
      simulated for each age/gender combination. A large number yields a lower
      stochastic variation in the risk factor histories. The upper practical limits
      depends on a number of factors, such as computer memory and the complexity
      of the simulated scenario."
    ),
    wrap_tooltip(
      numericInput(ns("starting_year"), "Starting year:", value = 2010),
      "The year in which the simulation starts. When newborns are used, the data
      should provide newborns for all years after this starting year."
    ),
    wrap_tooltip(
      numericInput(ns("years"), "Number of years:", value = 25),
      "The number of years DYNAMO projects the scenarios into the future."
    ),
    wrap_tooltip(
      numericInput(ns("time_step"), "Calculation time step:", value = 1),
      "Define the interval at which outcome measures are calculated."
    ),
    wrap_tooltip(
      numericInput(ns("random_seed"), "Random seed:", value = 1),
      "The value used to initialize the random number generator during the simulation.
      Any number will do, but when running two different simulations with exactly
      the same data, choosing the same random number ensures identical results.
      Otherwise, random variation slightly alters the simulation outcomes."
    ),
    wrap_tooltip(
      actionButton(ns("save"), "Save and Run", class = "btn-lg btn-success"),
      "Start the simulation."
    )
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
#' @export
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
