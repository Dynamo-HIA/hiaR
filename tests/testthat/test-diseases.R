
test_that("create_disease_dir creates correct directory structure and files", {
  withr::with_tempdir({
    disease_name <- "TestDisease"

    risk_factor_types <- c("continuous", "compound", "categorical")

    risk_factor_list <- lapply(risk_factor_types, function(type) {
      return(list(
        type = type,
        data = generate_relriskfromriskfactor_test_data(type)
      ))
    })

    names(risk_factor_list) <- paste0("RiskFactor", 1:length(risk_factor_types))

    diseases_list <- lapply(1:2, function(i) {
      return(list(
        data = generate_relriskfromdisease_test_data()
      ))
    })

    names(diseases_list) <- paste0("Disease", 1:length(diseases_list))

    populations <- c("Pop1", "Pop2")

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

    parameter_types <- c("Acutely Fatal", "Cured Fraction")

    excess_mortalities_list <- lapply(parameter_types, function(type) {
      return(list(
        unit_type = "Rate",
        parameter_type = type,
        data = generate_excessmortality_test_data()
      ))
    })

    names(excess_mortalities_list) <- populations

    result <- write_disease_dir(
      disease_name = disease_name,
      risk_factor_list = risk_factor_list,
      diseases_list = diseases_list,
      prevalences_list = prevalences_list,
      incidences_list = incidences_list,
      excess_mortalities_list = excess_mortalities_list,
      disability_list = prevalences_list
    )

    # Test assertions
    expect_true(result)
    expect_true(dir.exists(disease_name))
    expect_true(dir.exists(file.path(disease_name, "Relative_Risks_From_Risk_Factor")))
    expect_true(dir.exists(file.path(disease_name, "Relative_Risks_From_Diseases")))
    expect_true(dir.exists(file.path(disease_name, "Prevalences")))
    expect_true(dir.exists(file.path(disease_name, "Incidences")))
    expect_true(dir.exists(file.path(disease_name, "Excess_Mortalities")))
    expect_true(dir.exists(file.path(disease_name, "Disability")))

    expect_true(file.exists(file.path(disease_name, "Relative_Risks_From_Risk_Factor", "Relative_Risk_RiskFactor1_TestDisease-RiskFactor1.xml")))
    expect_true(file.exists(file.path(disease_name, "Relative_Risks_From_Diseases", "Relative_Risk_Disease1_TestDisease-Disease1.xml")))
    expect_true(file.exists(file.path(disease_name, "Prevalences", "Pop1_TestDisease_Prevalences.xml")))
    expect_true(file.exists(file.path(disease_name, "Incidences", "Pop1_TestDisease_Incidences.xml")))
    expect_true(file.exists(file.path(disease_name, "Excess_Mortalities", "Pop1_TestDisease_Mortalities.xml")))
    expect_true(file.exists(file.path(disease_name, "Disability", "Pop1_TestDisease_Disability.xml")))
  })
})
