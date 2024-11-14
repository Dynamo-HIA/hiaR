#' This creates the Shiny UI.
#'
#' @return dashboardPage A Shiny dashboardPage() element.
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' @import shinyFiles
#' @import stats
create.shiny.ui <- function(dynamodir, rundir, simdir, population, diseases, risk.factor, relative.risks) {

	ui <- fluidPage(
	  useShinyjs(),
	  #titlePanel("Simulation Configuration"),
	  navbarPage("DynamoHIA",
		 tabPanel("Program configuration",
				  fluidRow(
					textInput("dir_dynamo", "Dynamo folder", rundir),
					shinyDirButton("open_dynamo", "Set", "Choose a DYNAMO folder", multiple = FALSE)
				  ),
				  fluidRow(
					textInput("dir_working", "Working folder", dynamodir),
					shinyDirButton("open_working", "Set", "Choose a working folder", multiple = FALSE),
				  ),
				  fluidRow(
					textInput("dir_simulation", "Simulation folder", simdir),
					shinyDirButton("open", "Set", "Choose a simulation folder", multiple = FALSE),
				  ),
				  div(tags$label("Simulation:")),
				  fluidRow(
					actionButton("save", "Save and Run", class = "btn-lg btn-success")
				  ),
		 ),
		tabPanel("Simulation configuration",
				 textInput("simulation_name", "Simulation name:", ""),
				 selectInput("population", "Population:", population),
				 radioButtons("newborns", "Newborns:", c("yes", "no")),
				 numericInput("population_size", "Simulated population size:", value = 50),
				 numericInput("starting_year", "Starting year:", value = 2010),
				 numericInput("years", "Number of years:", value = 25),
				 numericInput("time_step", "Calculation time step:", value = 1),
				 numericInput("rnd_seed", "Random seed:", value = 1),
		),
		tabPanel("Risk Factor",
				 radioButtons("risk_factor", "Risk Factor:", risk.factor),
				 tableOutput('show_inputs')),
		tabPanel("Diseases",
				 do.call(fluidPage,
						 purrr::map(seq_len(nrow(diseases)), ~
									  fluidRow(
										column(4, checkboxInput(paste0(diseases$id[[.x]], "_enabled"), diseases$disease[[.x]], value = FALSE)),
										column(8,
											   selectInput(paste0(diseases$id[[.x]], "_prevalence"), "Prevalence:", choices=diseases$prevalence[[.x]]),
											   selectInput(paste0(diseases$id[[.x]], "_incidence"), "Incidence:", choices=diseases$incidence[[.x]]),
											   selectInput(paste0(diseases$id[[.x]], "_excess_mortality"), "Excess Mortality:", choices=diseases$excess.mortality[[.x]]),
											   selectInput(paste0(diseases$id[[.x]], "_disabling_impact"), "Disabling impact or DALYweight:", choices=diseases$disabling.impact[[.x]])
										)
									  )
						 )
				 )
		),
		tabPanel("Relative Risks",
				 do.call(fluidPage,
						 purrr::map(seq_len(nrow(relative.risks)), ~
									  fluidRow(id = relative.risks$id[[.x]],
											   column(4, checkboxInput(paste0(relative.risks$id[[.x]], "_enabled"),
																	   paste(relative.risks$from[[.x]], relative.risks$to[[.x]], sep=" to "), value = FALSE)),
											   column(8, selectInput(paste0(relative.risks$id[[.x]], "_filename"), "Relative Risk:",
																	 choices=c("None", relative.risks$fileName[[.x]])))
									  )
						 )
				 )
		),
		tabPanel("Scenarios",
				 uiOutput("reference"),
				 uiOutput("scenarios"),
				 actionButton("scenario_add", "Add Scenario"),
				 actionButton("scenario_remove", "Remove Scenario"),
		),
	  )
	)
	return(ui)
}
