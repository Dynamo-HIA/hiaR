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
single_risk_factor_ui <- function(id, risk_factor_name, risk_factor_files) {
  ns <- NS(id)

  bslib::card(
    class = "mb-3", # Add spacing between cards if multiple cards are stacked
    bslib::card_header(
      checkboxInput(ns("check"), risk_factor_name) # Checkbox input as the card title
    ),
    bslib::card_body(
      conditionalPanel(
        condition = paste0("input['", ns("check"), "']"),
        div(
          style = "margin-left: 20px;",
          selectInput(ns("prevalence"), "Prevalence",
            choices = names(risk_factor_files$Prevalences)
          ),
          selectInput(ns("transitions"), "Transitions",
            choices = names(risk_factor_files$Transitions)
          )
        )
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
      if (!input$check) {
        return(NULL)
      }
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
#' @keywords internal
#'
risk_factor_ui <- function(id, reference_data) {
  ns <- NS(id)

  risk_factors <- reference_data()$risk_factors

  tagList(
    lapply(seq_along(risk_factors), function(i) {
      single_risk_factor_ui(
        ns(paste0("risk_factor_", i)),
        risk_factor_name = names(risk_factors)[i],
        risk_factor_files = risk_factors[[i]]
      )
    })
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
#' @keywords internal
#'
risk_factor_server <- function(id, reference_data) {
  moduleServer(id, function(input, output, session) {
    risk_factor_names <- reactiveVal()
    risk_factor_servers <- reactiveValues(servers = list())
    server_name_prefix <- "risk_factor_"

    observeEvent(reference_data(), {
      new_data <- reference_data()$risk_factors
      risk_factor_names(names(new_data))

      risk_factor_servers$servers <- NULL
      lapply(seq_along(names(new_data)), function(i) {
        server_name <- paste0(server_name_prefix, i)
        risk_factor_servers$servers[[server_name]] <- single_risk_factor_server(server_name)
      })
    })

    user_data <- reactive({
      fetch_server_data(
        server_name_prefix = server_name_prefix,
        server_list = risk_factor_servers$servers,
        item_names = risk_factor_names()
      )
    })

    return(user_data)
  })
}
