
debug <- getShinyOption("debug")

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  navbarPage("DYNAMO-HIA",
             tabPanel("Configuration",
                      bslib::layout_columns(
                        bslib::card(
                          h2("Program configuration"),
                          p("Set the path to the working directory and to the
                            Dynamo executable."),
                          hiaR::program_config_ui("program_config")
                        ),
                        bslib::card(
                          h2("Simulation configuration"),
                          p("Define simulation properties."),
                          hiaR::simulation_config_ui("simulation_config")
                        )
                      )
             ),
             tabPanel("Risk Factors",
                      uiOutput("risk_factor_ui"),
                      verbatimTextOutput("selected_risk_factors_display")
                      ),
             tabPanel("Diseases",
                      uiOutput("disease_selection_ui"),
                      verbatimTextOutput("selected_diseases_display")
                      ),
             tabPanel("Relative Risks",
                      uiOutput("relative_risk_ui"),
                      verbatimTextOutput("selected_relative_risk_display")),
             tabPanel("Scenarios",
                      uiOutput("scenario_ui"),
                      verbatimTextOutput("selected_scenarios_display"))
  )
)

server <- function(input, output, session) {
  reference_data <- reactiveVal(NULL)
  user_paths <- reactiveValues(
    dynamo_path = NULL,
    working_path = NULL
  )
  available_relative_risks <- reactiveVal(NULL)

  user_program_config <- hiaR::program_config_server("program_config")

  observeEvent(user_program_config$reference_data(), {
    new_reference_data <- user_program_config$reference_data()
    reference_data(new_reference_data)
  })

  observeEvent(user_program_config$working_path(), {
    new_working_path <- user_program_config$working_path()
    user_paths$working_path <- new_working_path
  })

  observeEvent(user_program_config$dynamo_path(), {
    new_dynamo_path <- user_program_config$dynamo_path()
    user_paths$dynamo_path <- new_dynamo_path
  })

  output$disease_selection_ui <- renderUI({
    req(reference_data())
    hiaR::disease_selection_ui("selection1", reference_data)
  })

  selected_diseases <- hiaR::disease_selection_server("selection1", reference_data)

  output$risk_factor_ui <- renderUI({
    req(reference_data())
    hiaR::risk_factor_ui("risk_factors", reference_data)
  })

  selected_risk_factors <- hiaR::risk_factor_server("risk_factors", reference_data)

  output$relative_risk_ui <- renderUI({
    req(available_relative_risks())
    hiaR::relative_risk_ui("relative_risks", available_relative_risks)
  })

  selected_relative_risks <- hiaR::relative_risk_server(
    "relative_risks", available_relative_risks)

  output$scenario_ui <- renderUI({
    req(reference_data())
    hiaR::scenario_ui("scenarios")
  })
  selected_scenarios <- hiaR::scenario_server("scenarios", reference_data, selected_risk_factors, debug)

  # Update the choice options for relative risks ratios into diseases
  # depending on user input from selected risk factors and diseases
  observe({ # https://groups.google.com/g/shiny-discuss/c/vd_nB-BH8sw

    relative_risks <- reference_data()$relative_risks

    if (is.null(relative_risks)) { # re-set if no reference data
      available_relative_risks(NULL)
    } else {
      diseases <- names(selected_diseases())
      risk_factors <- names(selected_risk_factors())
      available_relative_risks(
        hiaR::filter_relative_risks(relative_risks, diseases, risk_factors)
      )
    }
  })

  user_simulation_config <- hiaR::simulation_config_server(
    "simulation_config",
    reference_data,
    selected_diseases,
    selected_risk_factors,
    selected_relative_risks,
    selected_scenarios,
    user_paths
  )

  # Display the reactive values returned by each element
  if (debug) {
    observe({
      output$selected_diseases_display <- renderPrint({ selected_diseases() })
      output$selected_risk_factors_display <- renderPrint({ selected_risk_factors() })
      output$selected_relative_risk_display <- renderPrint({ selected_relative_risks() })
      output$selected_scenarios_display <- renderPrint({ selected_scenarios() })
    })
  }

}

shinyApp(ui, server)
