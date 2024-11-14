

#' UI for selecting a single disease
#'
#' @param id A string. The id for the namespace in the module
#' @param disease_name The name of the disease, displayed in the card title
#' @param disease_files A named list of xml files the user can choose from.
#' The list has 4 elements:
#' \itemize{
#'  \item Prevalences
#'  \item Incidences
#'  \item Excess_Mortalities
#'  \item Disability
#' }
#' Each element itself is a named list, where the names are xml files.
#'
#' @returns A `bslib` card where the header is the disease name with a checkbox.
#' If the checkbox is clicked, the user sees the card content: `selectInput`
#' fields for prevalence, incidence, excess mortality and disability.
#'
single_disease_ui <- function(id, disease_name, disease_files) {
  ns <- NS(id)

  bslib::card(
    class = "mb-3", # Add spacing between cards if multiple cards are stacked
    bslib::card_header(
      checkboxInput(ns("check"), disease_name) # Checkbox input as the card title
    ),
    bslib::card_body(
      conditionalPanel(
        condition = paste0("input['", ns("check"), "']"),
        div(
          style = "margin-left: 20px;",
          selectInput(ns("prevalence"), "Prevalence",
                      choices = names(disease_files$Prevalences)),
          selectInput(ns("incidence"), "Incidence",
                      choices = names(disease_files$Incidences)),
          selectInput(ns("excess_mortality"), "Excess mortality",
                      choices = names(disease_files$Excess_Mortalities)),
          selectInput(ns("disability"), "Disabling Impact or DALYweight",
                      choices = names(disease_files$Disability))
        )
      )
    )
  )

}


#' Server part for a single disease
#'
#' @param id A string. The module namespace id, matching the `id` in the UI part.
#'
#' @returns A reactive expression. If the checkbox in the corresponding UI
#' is not checked, it returns `NULL`. Otherwise, it returns a list of 4:
#' \itemize{
#'  \item prevalence
#'  \item incidence
#'  \item excess_mortality
#'  \item disability
#' }
#' Each of the items contain the xml file the user chose in the UI.
#'
#' @export
#'
single_disease_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      if (!input$check) {
        return(NULL)
      }
      list(
        prevalence = input$prevalence,
        incidence = input$incidence,
        excess_mortality = input$excess_mortality,
        disability = input$disability
      )
    })
  })
}


#' UI for selecting multiple diseases
#'
#' Create a user interface for selecting multiple diseases and their associated
#' XML files for prevalence, incidence, excess mortality, and disability.
#'
#' @param id A string. The ID for the namespace in the module.
#' @param reference_data A reactive expression that returns a list containing disease data.
#'   The list should have a 'diseases' element, which is itself a list where each element
#'   represents a disease with its associated XML files.
#'
#' @returns A tagList containing UI elements for each disease. Each disease is represented
#'   by a `single_disease_ui` component.
#'
#' @details This function generates a UI for multiple diseases based on the provided
#'   reference data. For each disease in the reference data, it creates a `single_disease_ui`
#'   component, allowing users to select specific XML files for various disease parameters.
#'
#' @seealso \code{\link{single_disease_ui}}
#'
#'
#' @export
disease_selection_ui <- function(id, reference_data) {
  ns <- NS(id)

  diseases <- reference_data()$diseases

  tagList(
    lapply(seq_along(diseases), function(i) {
      single_disease_ui(
        ns(paste0("disease_", i)),
        disease_name = names(diseases)[i],
        disease_files = diseases[[i]]
      )
    })
  )
}


#' Server logic for selecting multiple diseases
#'
#' Handle the selection of multiple diseases and their associated parameters
#' (prevalence, incidence, excess mortality, and disability). Based on provided
#' reference data, the function dynamically creates and manages individual disease servers.
#'
#' @param id A string. The ID for the namespace in the module.
#' @param reference_data A reactive expression that returns a list containing disease data.
#'   The list should have a 'diseases' element, which is itself a list where each element
#'   represents a disease with its associated XML files.
#'
#' @returns A reactive expression that returns a list of selected diseases and their parameters.
#'   Each element in the list contains:
#'   \itemize{
#'     \item name: The original name of the disease from the reference data
#'     \item values: A list of selected XML files for prevalence, incidence, excess mortality, and disability
#'   }
#'   Only diseases that have been selected by the user (checkbox checked) are included in the output.
#'
#' @details
#' This server function dynamically creates individual disease servers for each disease in the
#' reference data. It responds to changes in the reference data, updating the available diseases
#' accordingly. The function collects and processes user selections for each disease, returning
#' a reactive expression with the compiled data.
#'
#' @seealso \code{\link{disease_selection_ui}}, \code{\link{single_disease_server}}
#'
#' @export
disease_selection_server <- function(id, reference_data) {
  moduleServer(id, function(input, output, session) {

    disease_inputs <- reactiveVal()
    disease_servers <- reactiveValues(servers = list())

    observeEvent(reference_data(), {
      new_diseases <- reference_data()$diseases
      disease_inputs(new_diseases)
      disease_servers$servers <- NULL
      lapply(seq_along(names(new_diseases)), function(i) {
        id_name <- paste0("disease_", i)
        disease_servers$servers[[id_name]] <- single_disease_server(id_name)
      })
    })


    # Display the selected outputs
    user_data <- reactive({
      outputs <- lapply(seq_along(names(disease_servers$servers)), function(i) {
        disease_name <- paste0("disease_", i)
        values <- tryCatch(disease_servers$servers[[disease_name]](), error = function(e) NULL)
        if (!is.null(values)) {
          list(
            name = names(disease_inputs())[i],
            values = values
          )
        } else {
          NULL
        }
      })

      Filter(Negate(is.null), outputs)
    })

   return(user_data)

  })
}
