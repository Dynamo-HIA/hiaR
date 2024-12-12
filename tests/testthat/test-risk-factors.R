test_that("create_risk_factor_dir creates correct directory structure and files", {
  withr::with_tempdir({
    risk_factor_name <- "TestRiskFactor"

    populations <- c("Pop1", "Pop2")
    matrix_type <- c("zero", "netto")

    transition_matrix_list <- lapply(1:length(populations), function(i) {
      return(list(
        type = matrix_type[i],
        data = generate_transitionmatrix_test_data()
      ))
    })

    names(transition_matrix_list) <- populations

    trend <- list(NULL, 0.1)

    transition_drift_list <- lapply(1:length(populations), function(i) {
      return(list(
        type = matrix_type[i],
        data = generate_transitiondrift_test_data(),
        trend = trend[[i]]
      ))
    })

    names(transition_drift_list) <- populations

    risk_factor_types <- c("continuous", "categorical", "compound")

    relative_risks_death_list <- lapply(risk_factor_types, function(type) {
      return(list(
        type = type,
        data = generate_relriskfromriskfactor_test_data(type)
      ))
    })

    names(relative_risks_death_list) <- risk_factor_types

    distributions <- c("Normal", "Log Normal")

    prevalences_list <- lapply(1:length(populations), function(i) {
      return(list(
        type = risk_factor_types[i],
        data = generate_riskfactorprevalences_test_data(mode = risk_factor_types[i]),
        distribution = distributions[i]
      ))
    })

    names(prevalences_list) <- populations

    prevalences_duration_list <- list(list(
      type = "duration",
      data = generate_riskfactorprevalences_test_data(mode = "duration")
    ))

    names(prevalences_duration_list) <- "duration"

    relative_risks_disability_list <- lapply(risk_factor_types, function(type) {
      return(list(
        type = type,
        data = generate_relriskfromriskfactor_test_data(type)
      ))
    })

    names(relative_risks_disability_list) <- risk_factor_types

    risk_factor_configuration <- generate_riskfactorconfiguration_test_data(mode = "continuous")

    result <- write_risk_factor_dir(
      risk_factor_name = risk_factor_name,
      transition_matrix_list = transition_matrix_list,
      transition_drift_list = transition_drift_list,
      relative_risks_death_list = relative_risks_death_list,
      prevalences_list = prevalences_list,
      prevalences_duration_list = prevalences_duration_list,
      relative_risks_disability_list = relative_risks_disability_list,
      risk_factor_configuration = risk_factor_configuration
    )

    # Test assertions
    expect_true(result)
    expect_true(dir.exists(risk_factor_name))
    expect_true(dir.exists(file.path(risk_factor_name, "Transitions")))
    expect_true(dir.exists(file.path(risk_factor_name, "Relative_Risks_For_Death")))
    expect_true(dir.exists(file.path(risk_factor_name, "Prevalences")))
    expect_true(dir.exists(file.path(risk_factor_name, "DurationDistributions")))
    expect_true(dir.exists(file.path(risk_factor_name, "Relative_Risks_For_Disability")))
    expect_true(file.exists(file.path(risk_factor_name, "configuration.xml")))
    # TODO: Add more assertions on file names
  })
})
