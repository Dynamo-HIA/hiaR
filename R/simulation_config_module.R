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
      textInput(ns("simulation_name"), "Simulation name:", "Simulation_1"),
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
      uiOutput(ns("save_button")),
      "Save the simulation configuration and start the simulation. This requires that paths to
      the working directory of the DYNAMO-HIA app are set in the program configuration and that at least
      one risk factor has been selected."
    ),
    wrap_tooltip(
      textOutput(ns("status")),
      "Status of the simulation. Shows whether the simulation was successful or has failed.
      In case of failure, check the console for details."
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
#' @param disease_configs A reactive, containing the disease configurations.
#' @param risk_factor_configs A reactive, containing the risk factor configurations.
#' @param relative_risk_configs A reactive, containing the relative risk configurations.
#' @param scenario_configs A reactive, containing the scenario configurations.
#' @param program_config A reactive, containing the program configurations.
#'
#' @returns A reactive list of key-value pairs read from the config UI, to be
#' used in the app/by other modules.
#'
#' @export
#' @keywords internal
#'
simulation_config_server <- function(id,
                                     reference_data,
                                     disease_configs,
                                     risk_factor_configs,
                                     relative_risk_configs,
                                     scenario_configs,
                                     program_config) {
  moduleServer(
    id,
    function(input, output, session) {
      status <- reactiveVal("Ready to run simulation")

      ready <- reactive({
        program_config$working_path != "" &&
          program_config$dynamo_path != "" &&
          input$simulation_name != "" &&
          length(risk_factor_configs()) > 0 &&
          status() != "Running simulation"
      })

      output$save_button <- renderUI({
        actionButton(
          session$ns("save"),
          "Save and Run",
          class = "btn-lg btn-success",
          disabled = !ready()
        )
      })

      user_data <- reactive({
        list(
          simulation_name = input$simulation_name,
          population = input$population,
          has_newborns = input$newborns == "yes",
          population_size = input$population_size,
          starting_year = input$starting_year,
          years = input$years,
          time_step = input$time_step,
          random_seed = input$random_seed
        )
      })

      observeEvent(
        input$save,
        {
          req(input$save)

          status("Running simulation")

          output$status <- renderText({
            status()
          })

          if (length(scenario_configs()) > 0) {
            scenarios <- lapply(names(scenario_configs()), function(name) {
              return(configure_scenario(
                name,
                # The file names must not have extensions
                fs::path_ext_remove(scenario_configs()[[name]]$transition),
                fs::path_ext_remove(scenario_configs()[[name]]$prevalence),
                scenario_configs()[[name]]$percent_population,
                scenario_configs()[[name]]$min_age,
                scenario_configs()[[name]]$max_age,
                scenario_configs()[[name]]$gender
              ))
            })
          } else {
            scenarios = list()
          }

          if (length(disease_configs()) > 0) {
            diseases <- lapply(names(disease_configs()), function(name) {
              return(configure_disease(
                name,
                fs::path_ext_remove(disease_configs()[[name]]$prevalence),
                fs::path_ext_remove(disease_configs()[[name]]$incidence),
                fs::path_ext_remove(disease_configs()[[name]]$excess_mortality),
                fs::path_ext_remove(disease_configs()[[name]]$disability)
              ))
            })
          } else {
            diseases = list()
          }

          risk_factors <- lapply(names(risk_factor_configs()), function(name) {
            return(configure_risk_factor(
              name,
              fs::path_ext_remove(risk_factor_configs()[[name]]$transitions),
              fs::path_ext_remove(risk_factor_configs()[[name]]$prevalence)
            ))
          })[[1]] # Can only have one risk factor!

          if (nrow(relative_risk_configs()) > 0) {
            relative_risks <- lapply(1:nrow(relative_risk_configs()), function(i) {
              return(configure_relative_risk(
                i,
                relative_risk_configs()[i, "from"],
                relative_risk_configs()[i, "to"],
                fs::path_ext_remove(relative_risk_configs()[i, "filename"])
              ))
            })
          } else {
            relative_risks = list()
          }

          create_simulation_dir(
            fs::path(program_config$working_path, "Simulations", user_data()$simulation_name),
            has_newborns = user_data()$has_newborns,
            starting_year = user_data()$starting_year,
            number_of_years = user_data()$years,
            population_size = user_data()$population_size,
            min_age = 0,
            max_age = 95,
            time_step = user_data()$time_step,
            ref_scenario_name = paste0(user_data()$simulation_name, "_Reference_Scenario"),
            random_seed = user_data()$random_seed,
            population_name = user_data()$population,
            scenarios = scenarios,
            diseases = diseases,
            risk_factors = risk_factors,
            relative_risks = relative_risks
          )

          batch_file_path <- fs::path(program_config$working_path, "simulationnames.txt")

          message(paste("Writing simulation to batch file:", batch_file_path))
          write(user_data()$simulation_name, batch_file_path)

          message(paste("Running dynamo_hia at: ", program_config$dynamo_path))
          errors <- hiaR::run_dynamo_hia(batch_file_path, program_config$dynamo_path)
          if (isTRUE(errors)) {
            message("Simulation successful!")
            status("Simulation successful")
          } else {
            message(errors)
            status("Simulation failed")
          }
        }
      )

      output$status <- renderText({
        status()
      })

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
