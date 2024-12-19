
#' Create UI for single scenario
#'
#' @param id A character string. The ID used to namespace the UI elements.
#' @param prevalence_and_transition_choices A reactive value that returns a list
#' of two character vector of choices. The names should be `prevalences` and
#' `transitions`.
#'
#' @returns A Shiny UI component for defining a scenario.
#' @keywords internal
#'
single_scenario_ui <- function(id, prevalence_and_transition_choices) {
  ns <- NS(id)

  prevalence_and_transition_choices <- prevalence_and_transition_choices()

  ui_min_age <- 0
  ui_max_age <- 95
  prevalence_choices <- prevalence_and_transition_choices$prevalences
  transition_choices <- prevalence_and_transition_choices$transitions

  fluidRow(column(4,
                  wrap_tooltip(
                    textInput(ns("scenario_name"),
                              "Scenario name:",
                              value = ""),
                    "Give this scenario a name."
                  ),
                  wrap_tooltip(
                    numericInput(ns("percent_population"),
                                 value = 100, min = 0, max = 100,
                                 "% of Population Reached:"),
                  "Define the percent the population that is reached by the
                  intervention."
                  ),
                  wrap_tooltip(
                    radioButtons(ns("gender"), "Gender:",
                                 choiceNames=c("male", "female", "male and female"),
                                 choiceValues=c(0, 1, 2),
                                 selected = 2),
                    "Define the gender targeted by the intervention."
                  )

  ),
  column(8,
         wrap_tooltip(
           numericInput(ns("min_age"),
                        "Min. Age:",
                        value = ui_min_age,
                        min = ui_min_age,
                        max = ui_max_age),
           "Define the minimum age of people targeted by the intervention."
         ),
         wrap_tooltip(
           numericInput(ns("max_age"),
                        "Max. Age:",
                        value = ui_max_age,
                        min = ui_min_age,
                        max = ui_max_age),
           "Define the maximum age of people targeted by the intervention."
         ),
         wrap_tooltip(
           selectInput(ns("prevalence"),
                       "Prevalence:",
                       choices = prevalence_choices),
           "The prevalence resulting from the intervention.",
           placement = "right"
         ),
         wrap_tooltip(
           selectInput(ns("transition"),
                       "Transition:",
                       choices = transition_choices),
           "The transitions resulting from the intervention.",
           placement = "right"
         )
  )
  )
}


#' Server logic for defining a single scenario
#'
#' @param id  A character string. The ID used to namespace the server-side logic.
#' It should match the id of the corresponding UI elements.
#' @param reference_data A reactive expression. The expression determines the return
#' value of the server.
#'
#' @returns If `reference_data` returns `NULL`, the server returns an empty list.
#' Otherwise, it returns a list where each element is taken from the UI inputs.
#' @keywords internal
#'
single_scenario_server <- function(id, reference_data) {
  moduleServer(id, function(input, output, session) {

    reactive({
      current_inputs <- list()
      if (!is.null(reference_data())) {
        current_inputs <- list(
          scenario_name = input$scenario_name,
          percent_population = input$percent_population,
          gender = as.integer(input$gender),
          min_age = input$min_age,
          max_age = input$max_age,
          prevalence = input$prevalence,
          transition = input$transition
        )
        return(current_inputs)
      }
    })
  })
}

#' Create UI for scenarios
#'
#' @param id A character string. The ID used to namespace the UI element.
#'
#' @returns A UI with 3 elements: an `actionButton` for adding and removing
#' scenarios, and a `tabsetPanel`. The panel can be populated with multiple
#' tabs, as handled by the server.
#' @export
#' @keywords internal
#'
scenario_ui <- function(id) {
  ns <- NS(id)

  tagList(
    p("Configure the alternative scenarios to be simulated.
      The reference scenario is fully specified by the chosen risk factors,
      the diseases, and the relative risks.
      You can add multiple alternative scenarios by clicking on the
      'Add scenario' button, and remove the latest scenario by clicking on the
      'Remove latest scenario' button.
      You must choose either a different transition dataset, or a different
      risk factor prevalence dataset from that chosen on the risk factor tab."
      # TODO: I think this we have not yet implemented -- see the `setdiff` call
      # in the old app
      # TODO: this probably needs more explanation.
      ),
    actionButton(ns("add_scenario"), "Add scenario"),
    actionButton(ns("remove_scenario"), "Remove last scenario"),
    tabsetPanel(id = ns("tabs"), type = "tabs")
  )
}


#' Server-side logic for defining multiple scenarios
#'
#' @param id  A character string. The ID used to namespace the server-side logic.
#' It should match the id of the corresponding UI element.
#' @param reference_data A reactive value that returns a list. One item in the
#' list must be named `risk_factors` and contain elements named `Transitions` and
#' `Prevalences`. The names of the values of these elements are passed on as
#' choice options to \link{single_scenario_server}.
#'
#' @returns A reactive expression. The expression returns a list with the user inputs
#' collected from all child servers. The list is created with \link{fetch_server_data}.
#'
#' @details
#' The server handles the following things:
#' \itemize{
#'  \item Updates choices for transitions and prevalences for defining scenarios
#'  \item Adds and removes a scenario tab when the user clicks on the corresponding
#'  buttons in the UI.
#'  \item Keeps track of active servers and their counts
#' }
#'
#' @export
#' @keywords internal
#'
scenario_server <- function(id, reference_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    scenario_count <- reactiveVal(0)
    scenario_servers <- reactiveValues(servers = list())
    server_name_prefix <- "scenario_"
    prevalence_and_incidence_choices <- reactiveVal(NULL)

    observeEvent(reference_data(), {
      transitions <- lapply(reference_data()$risk_factors, function(x) names(x[["Transitions"]]))
      prevalences <- lapply(reference_data()$risk_factors, function(x) names(x[["Prevalences"]]))
      prevalence_and_incidence_choices(list(
        transitions = transitions,
        prevalences = prevalences
      ))
    })

    observeEvent(input$add_scenario, {
      new_count <- scenario_count() + 1
      scenario_count(new_count)

      appendTab(
        inputId = "tabs",
        tabPanel(
          title = paste("Scenario", new_count),
          single_scenario_ui(ns(paste0(server_name_prefix, new_count)), prevalence_and_incidence_choices)
        ),
        select = TRUE
      )

      server_name <- paste0(server_name_prefix, new_count)
      scenario_servers$servers[[server_name]] <- single_scenario_server(
        server_name, reference_data
      )
    })

    observeEvent(input$remove_scenario, {

      old_count <- scenario_count()
      server_name <- paste0(server_name_prefix, old_count)
      removeTab(
        inputId = "tabs",
        target = paste("Scenario", old_count)
      )
      new_count <- max(old_count - 1, 0)
      if (new_count > 0) {
        current_server_names <- names(scenario_servers$servers)
        new_server_names <- current_server_names[c(1:new_count)]
        new_server_set <- sapply(new_server_names, function(x) {
          scenario_servers$servers[[x]]
        }, simplify = FALSE, USE.NAMES = TRUE)
      } else {
        new_server_set <- NULL
      }
      scenario_servers$servers <- new_server_set
      scenario_count(new_count)
    })

    user_data <- reactive({
      fetch_server_data(
        server_name_prefix = server_name_prefix,
        server_list = scenario_servers$servers,
        item_names = names(scenario_servers$servers)
      )
    })

    return(user_data)
  })
}



