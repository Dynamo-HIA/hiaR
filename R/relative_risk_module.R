#' Create UI for a single group of relative risks.
#'
#' Generate a UI component for a single relative risk group,
#' creating a card with a checkbox group.
#'
#' @param id A character string. The ID used to namespace the UI elements.
#' @param title A character string. The title of the group of relative risks.
#' @param choices A reactive expression returning a list of possible choices for the checkbox group.
#'
#' @returns A Shiny UI component (bslib::card) containing a checkbox group for the specified relative risk group.
#'
#' @keywords internal
single_relative_risk_ui <- function(id, title, choices) {
  ns <- NS(id)

  choices <- choices()

  bslib::card(
    class = "mb-3",
    bslib::card_body(
      checkboxGroupInput(ns("check_group"),
        label = title,
        choices = choices
      )
    )
  )
}


#' Server logic for a single group of relative risks
#'
#' Server-side logic for a single group of relative risks,
#' handling the selection of choices from the checkbox group.
#'
#' @param id A character string. The ID that matches the corresponding UI element.
#' @param choices A reactive expression returning a named list of available choices.
#'
#' @returns A reactive expression that returns the names
#' of the selected choices.
#'
#' @keywords internal
single_relative_risk_server <- function(id, choices) {
  moduleServer(id, function(input, output, session) {
    reactive({
      choice_labels <- names(choices())
      if (length(choice_labels) > 0 & !is.null(input$check_group)) {
        indexes <- as.integer(input$check_group)
        return(choice_labels[indexes])
      } else {
        return(list())
      }
    })
  })
}

#' Create UI for Relative Risk Selection
#'
#' Generate the complete UI for selecting relative risks from risk
#' factor to diseases and diseases to diseases.
#'
#' @param id A character string. The ID used to namespace the UI elements.
#' @param relative_risks A reactive expression returning the relative risks data.
#'
#' @returns A tagList containing UI components for selecting relative risks from
#' diseases and risk factors.
#'
#' @keywords internal
relative_risk_ui <- function(id, relative_risks) {
  ns <- NS(id)

  disease_choices <- reactiveVal(NULL)
  risk_factor_choices <- reactiveVal(NULL)
  death_choices <- reactiveVal(NULL)
  disability_choices <- reactiveVal(NULL)

  relative_risks <- relative_risks()

  choices <- create_choices_from_relative_risks(
    relative_risks,
    caller = "ui"
  )
  disease_choices(choices$from_diseases)
  risk_factor_choices(choices$from_risk_factors)
  death_choices(choices$to_death)
  disability_choices(choices$to_disability)

  tagList(
    single_relative_risk_ui(
      ns("from_diseases"), "From Diseases to Diseases", disease_choices
    ),
    single_relative_risk_ui(
      ns("from_risk_factors"), "From Risk Factors to Diseases", risk_factor_choices
    ),
    single_relative_risk_ui(
      ns("to_death"), "From Risk Factors to Death", death_choices
    ),
    single_relative_risk_ui(
      ns("to_disability"), "From Risk Factors to Disability", disability_choices
    )
  )
}

#' Server Logic for Relative Risk Selection
#'
#' Server-side logic for selecting relative risks for diseases.
#'
#' @param id A character string. The ID that matches the corresponding UI element.
#' @param relative_risks A reactive expression returning the relative risks data.
#'
#' @returns A reactive expression that returns a list
#' containing the names of the xml files corresponding to the selected
#' diseases and risk factors.
#'
#' @keywords internal
relative_risk_server <- function(id, relative_risks) {
  moduleServer(id, function(input, output, session) {
    disease_choices <- reactiveVal(NULL)
    risk_factor_choices <- reactiveVal(NULL)
    death_choices <- reactiveVal(NULL)
    disability_choices <- reactiveVal(NULL)

    relative_risk_df <- reactiveVal(NULL)

    observeEvent(relative_risks(), {
      current_relative_risks <- relative_risks()
      new_relative_risk_df <- do.call(rbind, current_relative_risks)
      rownames(new_relative_risk_df) <- NULL
      relative_risk_df(new_relative_risk_df)

      choices <- create_choices_from_relative_risks(
        current_relative_risks,
        caller = "server"
      )
      disease_choices(choices$from_diseases)
      risk_factor_choices(choices$from_risk_factors)
      death_choices(choices$to_death)
      disability_choices(choices$to_disability)
    })

    child_servers <- list(
      from_diseases = single_relative_risk_server(
        "from_diseases", disease_choices
      ),
      from_risk_factors = single_relative_risk_server(
        "from_risk_factors", risk_factor_choices
      ),
      to_death = single_relative_risk_server(
        "to_death", death_choices
      ),
      to_disability = single_relative_risk_server(
        "to_disability", disability_choices
      )
    )

    user_data <- reactive({
      filter_df_from_server_data(
        relative_risk_df(), child_servers, "filename"
      )
    })

    return(user_data)
  })
}
