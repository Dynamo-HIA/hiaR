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

create_simulation_risk_factors_xml <- function(parent_node, risk_factors) {
  risk_factors_node <- xml2::xml_add_child(parent_node, "riskfactors")
  if (length(risk_factors) > 0) {
    for (rf in risk_factors) {
      stopifnot(
        all(c("uniquename", "transfilename", "prevfilename") %in% names(rf))
      )

      rf_node <- xml2::xml_add_child(risk_factors_node, "riskfactor")
      xml2::xml_add_child(rf_node, "uniquename", rf$uniquename)
      xml2::xml_add_child(rf_node, "transfilename", rf$transfilename)
      xml2::xml_add_child(rf_node, "prevfilename", rf$prevfilename)
    }
  }
  return(risk_factors_node)
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
  # Input validation
  stopifnot(
    is.logical(has_newborns),
    is.numeric(starting_year),
    is.numeric(number_of_years),
    is.numeric(sim_pop_size),
    is.numeric(min_age), min_age >= 0, min_age <= 95,
    is.numeric(max_age), max_age >= 0, max_age <= 95,
    time_step == 1,
    is.numeric(random_seed),
    is.character(pop_file_name)
  )

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
  xml2::xml_add_child(root, "resultType", result_type)
  xml2::xml_add_child(root, "popFileName", pop_file_name)

  # Add complex elements using helper functions
  create_simulation_scenarios_xml(root, scenarios)
  create_simulation_diseases_xml(root, diseases)
  create_simulation_risk_factors_xml(root, risk_factors)
  create_simulation_relative_risks_xml(root, relative_risks)

  validate_xml_schema(doc, schema_name)

  return(doc)
}


create_simulation_dir <- function(simulation_name,
                                  simulation_config = list(
                                    has_newborns = FALSE,
                                    starting_year = NULL,
                                    number_of_years = NULL,
                                    sim_pop_size = NULL,
                                    min_age = NULL,
                                    max_age = NULL,
                                    time_step = 1,
                                    ref_scenario_name = NULL,
                                    random_seed = NULL,
                                    result_type = "",
                                    pop_file_name = NULL,
                                    scenarios = list(),
                                    diseases = list(),
                                    risk_factors = list(),
                                    relative_risks = list()
                                  )) {
  # Input validation
  stopifnot(
    is.character(simulation_name),
    is.list(simulation_config)
  )

  # Create main directory if it doesn't exist
  if (!dir.exists(simulation_name)) {
    dir.create(simulation_name, recursive = TRUE)
  }

  # Create simulation configuration XML
  xml2::write_xml(
    do.call(create_simulation_configuration_xml, simulation_config),
    file.path(simulation_name, "configuration.xml")
  )

  return(TRUE)
}
