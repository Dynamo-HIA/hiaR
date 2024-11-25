#' Runs the Shiny app.
#'
#' @import shiny
#'
#' @export
run.shiny.app <- function(working.directory, backend.directory) {
  if (!file.exists(working.directory)){
    stop("Working directory does not exist.")
  }
  if (!file.exists(backend.directory)){
    stop("Back-end directory does not exist.")
  }
  options(hiar_working_directory = working.directory)
  options(hiar_java_directory = backend.directory)

	dynamodir <- getOption("hiar_working_directory")
	rundir <- getOption("hiar_java_directory")
	simdir <- NULL
	conf <- getValidTree(dynamodir)
	population <- lapply(conf$populations, `[[`, "populationName") #c("Batavia")
	diseases <- tibble(
		disease = sapply(conf$diseases, `[[`, "diseaseName"),
		prevalence = sapply(conf$diseases, `[[`, "prevalence"),
		incidence = sapply(conf$diseases, `[[`, "incidence"),
		excess.mortality = sapply(conf$diseases, `[[`, "excessmortality"),
		disabling.impact = sapply(conf$diseases, `[[`, "disability")
	) %>%
	mutate(id = paste0("Disease", row_number()))


	risk.factor <- lapply(conf$riskfactors, `[[`, "riskfactorName") #c("Alcohol_cat5", "BMI_cat3", "BMI_cont", "Smoking_cat3", "Smoking_dur")
	risk.factor.prevalence <- setNames(lapply(conf$riskfactors, `[[`, "RFprevalence"), risk.factor)
	risk.factor.transition <- setNames(lapply(conf$riskfactors, `[[`, "transitionFiles"), risk.factor)
	relative.risks <- conf$relativeRisks %>%
		group_by(from, to) %>%
		summarize(fileName=list(unique(fileName)), .groups = 'drop') %>%
		mutate(id = paste0("RR", row_number()))

	relative.risk.other <- unique(conf$relativeRisks$from[!(conf$relativeRisks$from %in% risk.factor)])

  ui <- create.shiny.ui(dynamodir, rundir, simdir, population, diseases, risk.factor, relative.risks)
  server <- create.shiny.server(risk.factor.prevalence, risk.factor.transition, relative.risks, relative.risk.other)
  shinyApp(ui, server)
}
