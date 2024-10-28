#' Title and description.
#'
#' @import dplyr
#' @import shiny
#' @import shinyFiles
#' @import shinyjs
replace_default <- function(input, var, val) {
  val.current <- isolate(input[[var]])
  if (is.null(val.current))
    val.current <- val
  val.current
}

#' Title and description.
#'
#' @import dplyr
#' @import shiny
#' @import shinyFiles
#' @import shinyjs
make_ui_scenario <- function(risk.factor.prevalence, risk.factor.transition, id, input, save) {
  f.0 <- paste0(id, "_scenario_delete")
  f.1 <- paste0(id, "_scenario_name")
  f.2 <- paste0(id, "_scenario_pct_reached")
  f.3a <- paste0(id, "_scenario_age_min")
  f.3b <- paste0(id, "_scenario_age_max")
  f.3c <- paste0(id, "_scenario_gender")
  f.4 <- paste0(id, "_scenario_risk_factor_prevalence")
  f.5 <- paste0(id, "_scenario_risk_factor_transition")

  prevalence.choices <- risk.factor.prevalence[[input$risk_factor]]
  transition.choices <- risk.factor.transition[[input$risk_factor]]
  prevalence.choices <- setdiff(prevalence.choices, input$risk_factor_prevalence)
  transition.choices <- setdiff(transition.choices, input$risk_factor_transition)

  ui <- fluidRow(column(4,
                        textInput(f.1, "Name:", value = replace_default(input, f.1, id)),
                        numericInput(f.2, "% of Population Reached:", value = replace_default(input, f.2, 100)),
                        radioButtons(f.3c, "Gender:", choiceNames=c("male", "female", "male and female"),
                                     choiceValues=c(0,1,2), selected=replace_default(input, f.3c, 2))),
                 column(8,
                        numericInput(f.3a, "Min. Age:", value = replace_default(input, f.3a, 0), min=0, max=95),
                        numericInput(f.3b, "Max. Age:", value = replace_default(input, f.3b, 95), min=0, max=95),
                        selectInput(f.4, "Prevalence:", choices=prevalence.choices, selected=replace_default(input, f.4, NULL)),
                        selectInput(f.5, "Transition:", choices=transition.choices, selected=replace_default(input, f.5, NULL))),
  )

  return(ui)
}

#' This creates the Shiny server.
#'
#' @import dplyr
#' @import shiny
#' @import shinyFiles
#' @import shinyjs
create.shiny.server <- function(risk.factor.prevalence, risk.factor.transition, relative.risks, relative.risk.other) {
	server <- function(input, output, session) {

	  save <- reactiveValues(scenarios=character())
	  default <- reactiveValues()

	  # Dynamo executable directory
	  observeEvent(input$open_dynamo, {
		roots <- getVolumes()()
		shinyDirChoose(input, 'open_dynamo', roots=roots, filetypes=c(''))
		if(length(input$open_dynamo) <= 1) return({})
		file <- parseDirPath(roots, input$open_dynamo)
		updateTextInput(inputId = "dir_dynamo", value=file)
	  })

	  # Dynamo working directory
	  observeEvent(input$open_working, {
		roots <- getVolumes()()
		shinyDirChoose(input, 'open_working', roots=roots, filetypes=c(''))
		if(length(input$open_working) <= 1) return({})
		file <- parseDirPath(roots, input$open_working)
		updateTextInput(inputId = "dir_working", value=file)
	  })

	  # Simulation directory
	  observeEvent(input$open, {
	  })

	  observeEvent(input$risk_factor, {
		valid <- ((relative.risks$from == input$risk_factor) | (relative.risks$from %in% relative.risk.other))
		for (i in seq_len(nrow(relative.risks))) {
		  if (valid[[i]]) {
			shinyjs::show(relative.risks$id[[i]])
		  } else {
			shinyjs::hide(relative.risks$id[[i]])
		  }
		}

	  })

	  # Scenario (reference)
	  output$reference <- renderUI({
		age.min <- ifelse(is.null(input$age_min), 0, isolate(input$age_min))
		age.max <- ifelse(is.null(input$age_max), 95, isolate(input$age_max))
		fluidRow(column(4,textInput("reference_name", "Name:", "Reference Scenario")),
				 column(8, numericInput("age_min", "Min. Age:", value = age.min, min=0, max=95),
				 numericInput("age_max", "Max. Age:", value = age.max, min=0, max=95),
				 selectInput("risk_factor_prevalence", "Prevalence:", choices=risk.factor.prevalence[[input$risk_factor]]),
				 selectInput("risk_factor_transition", "Transition:", choices=risk.factor.transition[[input$risk_factor]]))
				 )
	  })


	  # Scenario (alternate)
	  output$scenarios <- renderUI({
		do.call(fluidPage,
				purrr::map(save$scenarios, ~ make_ui_scenario(risk.factor.prevalence, risk.factor.transition, .x, input, save))
		)
	  })

	  # Add/Remove Scenario
	  observeEvent(input$scenario_add, {
		add.scenario <- sprintf("Alternate Scenario %d", length(save$scenarios)+1)
		save$scenarios <- union(save$scenarios, add.scenario)
	  })
	  observeEvent(input$scenario_remove, {
		if (length(save$scenarios) > 0) {
		  del.scenario <- sprintf("Alternate Scenario %d", length(save$scenarios))
		  save$scenarios <- setdiff(save$scenarios, del.scenario)
		}
	  })


	  # Save
	  observeEvent(input$save, {
		message(input$dir_simulation)
		if (input$dir_simulation == "")
		  showNotification("Select simulation directory first", type='warning')

	  })
	}
	return(server)
}
