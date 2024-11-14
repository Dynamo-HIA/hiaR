
ui <- fluidPage(
  navbarPage("DYNAMO-HIA",
             tabPanel("Configuration",
                      bslib::layout_columns(
                        bslib::card(
                          h2("Program configuration"),
                          p("Settings for the program. ADD MORE DESCRIPTION"),
                          program_config_ui("program_config")
                        ),
                        bslib::card(
                          h2("Simulation configuration"),
                          p("Settings for the simulation"),
                          simulation_config_ui("simulation_config")
                        )
                      )
             ),
             tabPanel("Diseases",
                      uiOutput("disease_selection_ui"),
                      verbatimTextOutput("selected_diseases_display")
                      )

  )
)

server <- function(input, output, session) {
  reference_data <- reactiveVal(NULL)
  user_paths <- reactiveValues(
    dynamo_path = NULL,
    working_path = NULL
  )

  user_program_config <- program_config_server("program_config")
  user_simulation_config <- simulation_config_server("simulation_config", reference_data)

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
    disease_selection_ui("selection1", reference_data)
  })

  selected_diseases <- disease_selection_server("selection1", reference_data)

  # without this button, selected_diseases_display updates automatically
  #module_selection_inputs <- eventReactive(input$update_data, {
  #  selected_diseases()
  #})

  #observeEvent(input$update_data, {
  #  message("loaded")
  #  output$selected_diseases_display <- renderPrint({ selected_diseases() })
  #})

  # this is for debugging at the moment
  observeEvent(selected_diseases(), {
    output$selected_diseases_display <- renderPrint({ selected_diseases() })
  })

}

shinyApp(ui, server)
