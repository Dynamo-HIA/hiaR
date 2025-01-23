#' Create UI for a Single Risk Factor
#'
#' Generate a UI component for a single risk factor in a Shiny application.
#' It creates a card with a checkbox header and conditional inputs for prevalence
#' and transitions.
#'
#' @param id A character string. The ID used to namespace the UI elements.
#' @param risk_factor_name A character string. The name of the risk factor to be displayed.
#' @param risk_factor_files A list containing three named elements:
#'   \itemize{
#'     \item Prevalences: A named list of prevalence file options.
#'     \item Transitions: A named list of transition file options.
#'   }
#' @param parent_ns A function that returns the namespace of the parent module.
#'
#' @returns A Shiny UI component (bslib::card) for the specified risk factor.
#'
#' @details The function creates a collapsible card with a checkbox in the header.
#' When the checkbox is checked, it reveals select inputs for prevalence and
#' transitions. The options for these inputs are populated
#' from the provided risk_factor_files list.
#'
#' @keywords internal
#'
single_risk_factor_ui <- function(id, risk_factor_name, risk_factor_files, parent_ns) {
  ns <- NS(id)

  conditionalPanel(
    condition = paste0("input['", parent_ns("risk_factor"), "'] == '", risk_factor_name, "'"),
    bslib::card_body(
        selectInput(ns("prevalence"), "Prevalence",
                    choices = names(risk_factor_files$Prevalences),
                    width = "100%"
        ),
        selectInput(ns("transitions"), "Transitions",
                    choices = names(risk_factor_files$Transitions),
                    width = "100%"
      )
    )
  )
}


#' Server logic for a Single Risk Factor
#'
#' This function defines the server-side logic for a single risk factor in a Shiny application.
#' It processes user inputs for a specific risk factor and returns the selected values when the
#' risk factor is active.
#'
#' @param id A character string. The ID used to namespace the server logic, matching the ID used in the UI.
#'
#' @returns A reactive expression that returns either NULL (if the risk factor is not checked) or
#' a list containing the selected values for prevalence and transitions.
#'
#' The returned reactive expression can be used by parent modules to collect data
#' from multiple risk factors.
#'
#' @keywords internal
#'
single_risk_factor_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        prevalence = input$prevalence,
        transitions = input$transitions
      )
    })
  })
}


#' Create UI for Multiple Risk Factors
#'
#' Create a collection of individual risk factor UI elements based on the
#' provided reference data.
#'
#' @param id A character string. The ID used to namespace the UI elements.
#' @param reference_data A reactive expression that returns a list containing risk factor information.
#' The list should have a 'risk_factors' element, which is itself a list where each element
#' represents a risk factor and contains file options for prevalences and transitions.
#'
#' @returns A tagList containing UI components for each risk factor.
#'
#' @export
#' @keywords internal
#'
risk_factor_ui <- function(id, reference_data) {
  ns <- NS(id)

  risk_factors <- reference_data()$risk_factors

  tagList(
    p("Choose the risk factor to include in the simulation. Select a file for the prevalence
    of the risk factor and the transitions from the risk factor.
    The risk factor prevalence chosen here is the risk factor exposure in
    the baseline year in the reference scenario."),
    bslib::card(
      class = "mb-3", # Add spacing between cards if multiple cards are stacked
      radioButtons(ns("risk_factor"), "Risk Factor",
                   choices = names(risk_factors),
                   selected = character(0)
      ),
      lapply(seq_along(risk_factors), function(i) {
        single_risk_factor_ui(
          id = ns(paste0("risk_factor_", i)),
          risk_factor_name = names(risk_factors)[i],
          risk_factor_files = risk_factors[[i]],
          ns
        )
      })
    )
  )
}


#' Server logic for managing multiple risk factors
#'
#' Dynamically create and manage individual risk factor servers based on the provided reference data,
#' and collect user inputs for all active risk factors.
#'
#' @param id A character string. The ID used to namespace the server logic, matching the ID used in the UI.
#' @param reference_data A reactive expression that returns a list containing risk factor information.
#' The list should have a 'risk_factors' element, which is itself a list where each element
#' represents a risk factor and contains file options for prevalences and transitions.
#'
#' @returns A reactive expression that returns a named list of active risk factors and their selected values.
#' Each element in the list corresponds to an active risk factor and contains a nested list with
#' `prevalence' and 'transitions'.
#'
#' @export
#' @keywords internal
#'
risk_factor_server <- function(id, reference_data) {
  moduleServer(id, function(input, output, session) {
    risk_factor_names <- reactiveVal()
    risk_factor_servers <- reactiveValues(servers = list())
    server_name_prefix <- "risk_factor_"

    # Create a proper reactive for the selected risk factor
    selected_risk_factor <- reactive({
      input$risk_factor
    })

    observeEvent(reference_data(), {
      new_data <- reference_data()$risk_factors
      risk_factor_names(names(new_data))
      # Reset servers list
      risk_factor_servers$servers <- NULL

      # Create servers for each risk factor
      lapply(seq_along(names(new_data)), function(i) {
        server_name <- paste0(server_name_prefix, i)
        current_risk_factor <- names(new_data)[i]

        risk_factor_servers$servers[[server_name]] <- single_risk_factor_server(server_name)
      })
    })

    user_data <- reactive({
      fetch_server_data(
        server_name_prefix = server_name_prefix,
        server_list = risk_factor_servers$servers,
        item_names = risk_factor_names()
      )[selected_risk_factor()]
    })

    return(user_data)
  })
}
