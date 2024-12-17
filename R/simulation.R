create_simulation_scenarios_xml <- function(parent_node, scenarios) {
  scenarios_node <- xml2::xml_add_child(parent_node, "scenarios")
  if (length(scenarios) > 0) {
    for (scenario in scenarios) {
      stopifnot(
        all(c(
          "uniquename", "successRate", "targetMinAge", "targetMaxAge",
          "targetSex", "transfilename", "prevfilename"
        ) %in% names(scenario)),
        scenario$successRate >= 0, scenario$successRate <= 100,
        scenario$targetMinAge >= 0, scenario$targetMinAge <= 95,
        scenario$targetMaxAge >= 0, scenario$targetMaxAge <= 95,
        scenario$targetSex >= 0, scenario$targetSex <= 2
      )

      scenario_node <- xml2::xml_add_child(scenarios_node, "scenario")
      xml2::xml_add_child(scenario_node, "uniquename", scenario$uniquename)
      xml2::xml_add_child(scenario_node, "successRate", scenario$successRate)
      xml2::xml_add_child(scenario_node, "targetMinAge", scenario$targetMinAge)
      xml2::xml_add_child(scenario_node, "targetMaxAge", scenario$targetMaxAge)
      xml2::xml_add_child(scenario_node, "targetSex", scenario$targetSex)
      xml2::xml_add_child(scenario_node, "transfilename", scenario$transfilename)
      xml2::xml_add_child(scenario_node, "prevfilename", scenario$prevfilename)
    }
  }
  return(scenarios_node)
}

create_simulation_diseases_xml <- function(parent_node, diseases) {
  diseases_node <- xml2::xml_add_child(parent_node, "diseases")
  if (length(diseases) > 0) {
    for (disease in diseases) {
      stopifnot(
        all(c(
          "uniquename", "prevfilename", "incfilename",
          "excessmortfilename", "dalyweightsfilename"
        ) %in% names(disease))
      )

      disease_node <- xml2::xml_add_child(diseases_node, "disease")
      xml2::xml_add_child(disease_node, "uniquename", disease$uniquename)
      xml2::xml_add_child(disease_node, "prevfilename", disease$prevfilename)
      xml2::xml_add_child(disease_node, "incfilename", disease$incfilename)
      xml2::xml_add_child(disease_node, "excessmortfilename", disease$excessmortfilename)
      xml2::xml_add_child(disease_node, "dalyweightsfilename", disease$dalyweightsfilename)
    }
  }
  return(diseases_node)
}

create_simulation_risk_factor_xml <- function(parent_node, risk_factor) {
  risk_factor_node <- xml2::xml_add_child(parent_node, "riskfactors")

  stopifnot(
    all(c("uniquename", "transfilename", "prevfilename") %in% names(risk_factor))
  )

  risk_factor_node <- xml2::xml_add_child(risk_factor_node, "riskfactor")
  xml2::xml_add_child(risk_factor_node, "uniquename", risk_factor$uniquename)
  xml2::xml_add_child(risk_factor_node, "transfilename", risk_factor$transfilename)
  xml2::xml_add_child(risk_factor_node, "prevfilename", risk_factor$prevfilename)

  return(risk_factor_node)
}

create_simulation_relative_risks_xml <- function(parent_node, rrs) {
  rrs_node <- xml2::xml_add_child(parent_node, "RRs")
  if (length(rrs) > 0) {
    for (rr in rrs) {
      stopifnot(
        all(c("RRindex", "isRRfrom", "isRRFile") %in% names(rr))
      )

      rr_node <- xml2::xml_add_child(rrs_node, "RR")
      xml2::xml_add_child(rr_node, "RRindex", rr$RRindex)
      xml2::xml_add_child(rr_node, "isRRfrom", rr$isRRfrom)
      if (!is.null(rr$isRRto)) {
        xml2::xml_add_child(rr_node, "isRRto", rr$isRRto)
      }
      xml2::xml_add_child(rr_node, "isRRFile", rr$isRRFile)
    }
  }
  return(rrs_node)
}

create_simulation_configuration_xml <- function(has_newborns = FALSE,
                                                starting_year,
                                                number_of_years,
                                                sim_pop_size,
                                                min_age,
                                                max_age,
                                                time_step = 1,
                                                ref_scenario_name = NULL,
                                                random_seed,
                                                result_type = "",
                                                pop_file_name,
                                                scenarios = list(),
                                                diseases = list(),
                                                risk_factors = list(),
                                                relative_risks = list()) {
  schema_name <- "simulation"

  # Create root node
  doc <- xml2::xml_new_document()
  root <- xml2::xml_add_child(doc, schema_name)

  # Add basic elements
  xml2::xml_add_child(root, "hasnewborns", if (has_newborns) "true" else "false")
  xml2::xml_add_child(root, "startingYear", starting_year)
  xml2::xml_add_child(root, "numberOfYears", number_of_years)
  xml2::xml_add_child(root, "simPopSize", sim_pop_size)
  xml2::xml_add_child(root, "minAge", min_age)
  xml2::xml_add_child(root, "maxAge", max_age)
  xml2::xml_add_child(root, "timeStep", time_step)

  if (!is.null(ref_scenario_name)) {
    xml2::xml_add_child(root, "refScenarioName", ref_scenario_name)
  }

  xml2::xml_add_child(root, "randomSeed", random_seed)
  xml2::xml_add_child(root, "resultType", result_type) # Not used by Java application
  xml2::xml_add_child(root, "popFileName", pop_file_name)

  # Add complex elements using helper functions
  create_simulation_scenarios_xml(root, scenarios)
  create_simulation_diseases_xml(root, diseases)
  create_simulation_risk_factor_xml(root, risk_factors)
  create_simulation_relative_risks_xml(root, relative_risks)

  validate_xml_schema(doc, schema_name)

  return(doc)
}

#' Create a simulation directory with configuration file
#'
#' Creates a new directory for a simulation and generates a configuration XML file.
#' The configuration is read by the DYNAMO-HIA model to run the configured simulation. Simulation
#' results will be stored in the same directory.
#' The function overwrites existing configuration files.
#'
#' @param simulation_name A character string with the name of the simulation directory to be created.
#' @param population_name A character string with the name of the population directory to be used.
#' @param starting_year A numeric indicating the year in which the simulation starts.
#' @param number_of_years A numeric indicating the duration of the simulation in years.
#' @param population_size A Numeric indicating the size of the population to simulate.
#' @param ref_scenario_name A character string with the name of the reference scenario.
#' @param has_newborns A logical for whether to include newborns in the simulation.
#' Default is `FALSE`.
#' @param min_age A numeric indicating the minimum age for the population (0-95). Default is 0.
#' @param max_age A numeric indicating the maximum age for the population (0-95). Default is 95.
#' @param time_step A numeric indicating the time step for the simulation. Must be 1.
#' @param random_seed An optional seed for random number generation.
#' @param scenarios A list of scenario configurations. Each scenario must be a list containing:
#'   \itemize{
#'     \item uniquename: A character string which uniquely identifies the scenario
#'     \item successRate: A numeric which specifies the success rate between 0 and 100
#'     \item targetMinAge: A numeric which sets the minimum target age (0-95)
#'     \item targetMaxAge: A numeric which sets the maximum target age (0-95)
#'     \item targetSex: A numeric which indicates the target sex code (0: male; 1: female; 2: both)
#'     \item transfilename: A character string which specifies the transition filename
#'     \item prevfilename: A character string which specifies the prevalence filename
#'   }
#'
#' @param diseases A list of disease configurations. Each disease must be a list containing:
#'   \itemize{
#'     \item uniquename: A character string which uniquely identifies the disease
#'     \item prevfilename: A character string which specifies the prevalence filename
#'     \item incfilename: A character string which specifies the incidence filename
#'     \item excessmortfilename: A character string which specifies the excess mortality filename
#'     \item dalyweightsfilename: A character string which specifies the disability weights filename
#'   }
#'
#' @param risk_factors A list containing a single risk factor configuration with:
#'   \itemize{
#'     \item uniquename: A character string which uniquely identifies the risk factor
#'     \item transfilename: A character string which specifies the transition filename
#'     \item prevfilename: A character string which specifies the prevalence filename
#'   }
#'
#' @param relative_risks A list of relative risk configurations. Each relative risk must be a list
#' containing:
#'   \itemize{
#'     \item RRindex: A character string which identifies the relative risk relationship
#'     \item isRRfrom: A character string which specifies the source. Must be the name of a
#'     disease or risk factor
#'     \item isRRto: A character string which specifies the target identifier. Must be the name of a
#'     disease or risk factor
#'     \item isRRFile: A character string which specifies the relative risk file path
#'   }
#'
#' @return Logical TRUE if directory creation and configuration was successful.
#'
#' @details
#' The function creates a directory with the specified simulation name and generates
#' a `configuration.xml` file inside it using the provided parameters.
#'
#' @export
create_simulation_dir <- function(simulation_name,
                                  population_name,
                                  starting_year,
                                  number_of_years,
                                  population_size,
                                  ref_scenario_name,
                                  has_newborns = FALSE,
                                  min_age = 0,
                                  max_age = 95,
                                  time_step = 1,
                                  random_seed = NULL,
                                  scenarios = list(),
                                  diseases = list(),
                                  risk_factors = list(),
                                  relative_risks = list()) {
  stopifnot(
    is.character(simulation_name),
    is.logical(has_newborns),
    is.numeric(starting_year),
    is.numeric(number_of_years),
    is.numeric(population_size),
    is.numeric(min_age), min_age >= 0, min_age <= 95,
    is.numeric(max_age), max_age >= 0, max_age <= 95,
    time_step == 1,
    is.numeric(random_seed),
    is.character(population_name)
  )

  # Create main directory if it doesn't exist
  if (!dir.exists(simulation_name)) {
    dir.create(simulation_name, recursive = TRUE)
  }

  # Create simulation configuration XML
  xml2::write_xml(
    create_simulation_configuration_xml(
      has_newborns = has_newborns,
      starting_year = starting_year,
      number_of_years = number_of_years,
      sim_pop_size = population_size,
      min_age = min_age,
      max_age = max_age,
      time_step = time_step,
      ref_scenario_name = ref_scenario_name,
      random_seed = random_seed,
      pop_file_name = population_name,
      scenarios = scenarios,
      diseases = diseases,
      risk_factors = risk_factors,
      relative_risks = relative_risks
    ),
    file.path(simulation_name, "configuration.xml")
  )

  return(TRUE)
}
