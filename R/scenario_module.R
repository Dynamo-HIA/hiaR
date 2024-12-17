
# understand logic for prevalence and transition
# (TODO: why does it work without tagList? is tagList not always necessary?)
# TODO: "remove" the tab with a button?
# TODO: the reactivity is not working. what to do with the ns's?
single_scenario_ui <- function(id, prevalence_and_transition_choices) {
  ns <- NS(id)

  prevalence_and_transition_choices <- prevalence_and_transition_choices()

  ui_min_age <- 0
  ui_max_age <- 95
  prevalence_choices <- prevalence_and_transition_choices$prevalences
  transition_choices <- prevalence_and_transition_choices$transitions

  fluidRow(column(4,
                  textInput(ns("scenario_name"),
                            "Scenario name:",
                            value = ""),
                  numericInput(ns("percent_population"),
                               "% of Population Reached:",
                               value = 100, min = 0, max = 100),
                  radioButtons(ns("gender"), "Gender:",
                               choiceNames=c("male", "female", "male and female"),
                               choiceValues=c(0, 1, 2),
                               selected = 2)
           ),
           column(8,
                  numericInput(ns("min_age"),
                               "Min. Age:",
                               value = ui_min_age,
                               min = ui_min_age,
                               max = ui_max_age),
                  numericInput(ns("max_age"),
                               "Max. Age:",
                               value = ui_max_age,
                               min = ui_min_age,
                               max = ui_max_age),
                  selectInput(ns("prevalence"),
                              "Prevalence:",
                              choices = prevalence_choices),
                  selectInput(ns("transition"),
                              "Transition:",
                              choices = transition_choices)
           )
  )
}


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
      }
      return(current_inputs)
    })

  })
}




scenario_ui <- function(id, reference_data) {
  ns <- NS(id)

  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(src = "script.js", type="text/javascript")
    ),
    br(),
    fluidRow(
      column(8, uiOutput(ns("ui_tabs"))),
    )
  )
}


scenario_server <- function(id, reference_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$NS

    prevalence_and_incidence_choices <- reactiveVal(NULL)

    observeEvent(reference_data(), {
      transitions <- lapply(reference_data()$risk_factors, function(x) names(x[["Transitions"]]))
      prevalences <- lapply(reference_data()$risk_factors, function(x) names(x[["Prevalences"]]))
      prevalence_and_incidence_choices(list(
        transitions = transitions,
        prevalences = prevalences
      ))
    })

    rv <- reactiveValues(
      scenario_count = 0,
      scenario_names = list(),
      trigger_add_scenario_button = FALSE,
      return_data = list()
    )

    add_scenario <- function() {
      rv$scenario_count <- rv$scenario_count + 1
      scenario_name <- paste0("scenario_", rv$scenario_count)
      rv$scenario_names[[length(rv$scenario_names) + 1]] <- scenario_name
      rv$return_data[[scenario_name]] <<- single_scenario_server(
        id = ns(scenario_name), reference_data
      )
      appendTab(inputId = "tab_scenario",
                tabPanel(
                  title = tab_title(scenario_name),
                  value = scenario_name,
                  single_scenario_ui(ns(scenario_name), prevalence_and_transition_choices)
                ))
    }

    # tab title with close button
    tab_title <- function(name, type = "scenario") {
      tags$span(
        name,
        tags$span(icon("times"),
                  style = "margin-left: 5px;",
                  onclick = paste0("Shiny.setInputValue(\"", paste0("remove_", type, "_tab"), "\", \"", name, "\", {priority: \"event\"})"))
      )
    }

    ## tabs
    output$ui_tabs <- renderUI({
      isolate({
        rv$scenario_count <- rv$scenario_count + 1
        scenario_name <- paste0("scenario_", rv$scenario_count)
        rv$scenario_names[[length(rv$scenario_names) + 1]] <- scenario_name
        rv$return_data[[scenario_name]] <<- single_scenario_server(
          id = scenario_name, reference_data
        )
        rv$trigger_add_scenario_button <- TRUE
      })
      tabsetPanel(id = "tab_scenario",
                  tabPanel(title = tab_title(scenario_name),
                           value = scenario_name,
                           single_scenario_ui(scenario_name, prevalence_and_incidence_choices))
                  )
    })

    ## add a button to the tabPanel
    observeEvent(rv$trigger_add_scenario_button, {
      if (rv$trigger_add_scenario_button) {
        rv$trigger_add_scenario_button <- FALSE
        shinyjs::delay(100, session$sendCustomMessage(type = "addbutton", list(id = "tab_scenario", trigger = "add_scenario")))
        tryCatch(o_data$destroy(),
                 error = function(e) NULL)
        o_data <<- observeEvent(input$add_scenario, {
          add_scenario()
        }, ignoreInit = TRUE)
      }
    }, once = FALSE)


    ## remove a scenario
    observeEvent(input$remove_scenario_tab, {
      removeTab(inputId = "tab_scenario", target = input$remove_scenario_tab)
      isolate({rv$scenario_names <- rv$scenario_names[!rv$scenario_names == input$remove_scenario_tab]})
    })

    user_data <- reactive({
      message("user_data updated")
      use_data <- rv$return_data[unlist(rv$scenario_names)]
      l_data <- list()
      if (length(use_data) > 0) {
        l_data <- lapply(seq_along(use_data), function(i) {
          data <- use_data[[i]]
          if (length(data()) > 0) {
            data()
          } else {
            list()
          }
        })
      }
      l_data
    })

    return(user_data)
  })
}



