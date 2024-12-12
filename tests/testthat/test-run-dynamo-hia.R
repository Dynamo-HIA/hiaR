# This is an integration test of the run_dynamo_hia function which wraps the Java application.
# It first creates dummy XML config files for risk factors, diseases, populations, and a simulation.
# Then it uses the run_dynamo_hia function to run the Java application with these files.
# For this test to run, the path to the Java application must be set in the environment variable
# `DYNAMO_HIA_PATH`.
test_that("run_dynamo_hia runs without error", {
  # Run this in a temporary directory to leave no traces
  withr::with_tempdir({
    # Create reference data directory
    fs::dir_create("Reference_Data")
    withr::with_dir("Reference_Data", {
      populations <- c("Netherlands", "France")

      risk_factors <- list(
        "AddedSugar" = "continuous",
        "PUFA" = "categorical",
        "TFA" = "compound"
      )

      diseases <- c("Diabetes_Mellitus_Type_2", "Ischemic_Heart_Disease")

      sex_ratio <- 1.0
      starting_year <- 2009
      ending_year <- 2012

      # Create populations subdirectory
      fs::dir_create("Populations")

      # Disability and mortality data for each population are needed later for diseases
      # because the disease mortalities must be lower than the overall population mortalities
      disability_list <- list()
      mortality_list <- list()

      withr::with_dir("Populations", {
        for (population in populations) {
          newborns_df <- create_newborns_test_data(sex_ratio, starting_year, ending_year)
          size_df <- generate_populationsize_test_data()
          mortality_df <- generate_diseaseprevalences_test_data(incidence = TRUE)
          disability_df <- generate_diseaseprevalences_test_data(incidence = FALSE)

          result <- create_population_dir(
            population,
            sex_ratio,
            starting_year,
            newborns_df,
            size_df,
            mortality_df,
            disability_df
          )
          expect_true(result)
          # Reduce the disability by 20% to make sure the disability is lower for diseases
          # than the population
          disability_df$percent <- pmax(disability_df$percent - 20.0, 0.0)
          disability_list[[population]][["data"]] <- disability_df
          mortality_list[[population]][["data"]] <- mortality_df
        }
      })

      names(disability_list) <- populations
      names(mortality_list) <- populations

      # Create transition matrices for risk factors
      matrix_types <- c("zero", "netto")

      transition_matrix_list <- lapply(matrix_types, function(type) {
        return(list(
          type = type,
          data = generate_transitionmatrix_test_data(num_cat = 3)
        ))
      })

      names(transition_matrix_list) <- matrix_types

      transition_drift_list <- lapply(matrix_types, function(type) {
        return(list(
          type = type,
          data = generate_transitiondrift_test_data(),
          trend = if (type == "netto") 0.1 else NULL
        ))
      })

      names(transition_drift_list) <- matrix_types

      # Create a risk factor subdirectory
      fs::dir_create("Risk_Factors")

      withr::with_dir("Risk_Factors", {
        for (risk_factor in names(risk_factors)) {
          risk_factor_type <- risk_factors[[risk_factor]]

          relative_risks_death_list <- lapply(seq_along(populations), function(i) {
            return(list(
              type = risk_factor_type,
              data = generate_relriskfromriskfactor_test_data(risk_factor_type, num_cat = 3)
            ))
          })

          names(relative_risks_death_list) <- populations

          prevalences_list <- lapply(seq_along(populations), function(i) {
            return(list(
              type = if (risk_factor_type != "compound") risk_factor_type else "categorical",
              data = generate_riskfactorprevalences_test_data(mode = if (risk_factor_type != "compound") risk_factor_type else "categorical", num_cat = 3),
              distribution = "Normal"
            ))
          })

          names(prevalences_list) <- populations

          prevalences_duration_list <- list(list(
            type = "duration",
            data = generate_riskfactorprevalences_test_data(mode = "duration")
          ))

          names(prevalences_duration_list) <- "duration"

          relative_risks_disability_list <- lapply(seq_along(populations), function(i) {
            return(list(
              type = risk_factor_type,
              data = generate_relriskfromriskfactor_test_data(risk_factor_type, num_cat = 3)
            ))
          })

          names(relative_risks_disability_list) <- populations

          risk_factor_configuration <- generate_riskfactorconfiguration_test_data(mode = risk_factor_type, num_cat = 3)

          result <- create_risk_factor_dir(
            risk_factor_name = risk_factor,
            transition_matrix_list = transition_matrix_list,
            transition_drift_list = transition_drift_list,
            relative_risks_death_list = relative_risks_death_list,
            prevalences_list = prevalences_list,
            prevalences_duration_list = prevalences_duration_list,
            relative_risks_disability_list = relative_risks_disability_list,
            risk_factor_configuration = risk_factor_configuration
          )
          expect_true(result)
        }
      })

      # Create a diseases subdirectory
      fs::dir_create("Diseases")

      withr::with_dir("Diseases", {
        for (disease in diseases) {
          risk_factor_list <- lapply(risk_factors, function(type) {
            return(list(
              type = type,
              data = generate_relriskfromriskfactor_test_data(type, num_cat = 3)
            ))
          })

          names(risk_factor_list) <- names(risk_factors)

          diseases_list <- list()

          prevalences_list <- lapply(1:length(populations), function(i) {
            return(list(
              data = generate_diseaseprevalences_test_data()
            ))
          })

          names(prevalences_list) <- populations

          incidences_list <- lapply(1:length(populations), function(i) {
            return(list(
              data = generate_diseaseprevalences_test_data(incidence = TRUE)
            ))
          })

          names(incidences_list) <- populations

          # Set the disease excess mortality lower than the overall population mortalities
          excess_mortalities_list <- lapply(populations, function(population) {
            df <- mortality_list[[population]][["data"]]
            value <- df$value
            df <- df[, c("age", "sex")]
            df$unit <- runif(nrow(df), 0, 10)
            df$acutelyfatal <- pmax((value / 100.0) - 0.8, 0.0)
            df$curedfraction <- 0
            return(list(
              data = df,
              unit_type = "Rate",
              parameter_type = "Acutely Fatal"
            ))
          })

          names(excess_mortalities_list) <- populations

          result <- create_disease_dir(
            disease_name = disease,
            risk_factor_list = risk_factor_list,
            diseases_list = diseases_list,
            prevalences_list = prevalences_list,
            incidences_list = incidences_list,
            excess_mortalities_list = excess_mortalities_list,
            disability_list = disability_list
          )
          expect_true(result)
        }
      })
    })

    # Create a simulations directory
    fs::dir_create("Simulations")

    withr::with_dir("Simulations", {
      for (risk_factor in names(risk_factors)) {
        for (population in populations) {
          # Continuous risk factors need a drift transition matrix
          if (risk_factors[[risk_factor]] == "continuous") {
            transition_filename <- paste0("Transition_Drift_netto_", risk_factor, "_Netto")
          } else {
            transition_filename <- paste0("Transition_netto_", risk_factor, "_Netto")
          }
          # Create lists with filenames of XML configs of risk factors, diseases, populations
          scenario_configs <- list(configure_scenario(
            name = paste(population, risk_factor, "scenario", sep = "_"),
            success_rate = 80,
            min_age = 20,
            max_age = 50,
            gender = 1,
            transition_filename = transition_filename,
            prevalence_filename = paste(population, risk_factor, "Prevalences", sep = "_")
          ))

          disease_configs <- lapply(diseases, function(disease) {
            return(configure_disease(
              name = disease,
              prevalence_filename = paste(population, disease, "Prevalences", sep = "_"),
              incidence_filename = paste(population, disease, "Incidences", sep = "_"),
              excess_mortality_filename = paste(population, disease, "Mortalities", sep = "_"),
              disability_weights_filename = paste(population, disease, "Disability", sep = "_")
            ))
          })

          risk_factor_config <- configure_risk_factor(
            name = risk_factor,
            transition_filename = transition_filename,
            prevalence_filename = paste(population, risk_factor, "Prevalences", sep = "_")
          )

          relative_risk_config <- lapply(seq_along(diseases), function(i) {
            return(configure_relative_risk(
              index = i, # Start with zero
              from = risk_factor,
              to = diseases[i],
              relative_risk_filename = paste0(
                "Relative_Risk_", risk_factor, "_", diseases[i],
                "-", risk_factor
              )
            ))
          })

          create_simulation_dir(
            population,
            has_newborns = TRUE,
            starting_year = starting_year,
            number_of_years = ending_year - starting_year,
            population_size = 1,
            min_age = 0,
            max_age = 95,
            time_step = 1,
            ref_scenario_name = paste0(population, "_Reference_Scenario"),
            random_seed = 42,
            result_type = "Emptyness2BFilled",
            population_name = population,
            scenarios = scenario_configs,
            diseases = disease_configs,
            risk_factors = risk_factor_config,
            relative_risks = relative_risk_config
          )
        }
      }
    })

    write(paste(populations, collapse = "\n"), "simulationnames.txt")

    skip_on_ci() # TODO: Cannot run on CI because it requires Dynamo-HIA

    result <- run_dynamo_hia("simulationnames.txt", Sys.getenv("DYNAMO_HIA_PATH"))

    expect_true(result)
  })
})
