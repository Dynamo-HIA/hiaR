
# understand logic for prevalence and transition
# (TODO: why does it work without tagList? is tagList not always necessary?)
# TODO: "remove" the tab with a button?

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
        return(current_inputs)
      }
    })
  })
}

scenario_ui <- function(id, reference_data) {
  ns <- NS(id)

  tagList(
    actionButton(ns("add_scenario"), "Add Scenario"),
    actionButton(ns("remove_scenario"), "Remove last scenario"),
    tabsetPanel(id = ns("tabs"), type = "tabs")
  )
}


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



