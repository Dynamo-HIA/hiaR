test_that("create_population_dir creates correct directory structure and files", {
  withr::with_tempdir({
    # Mock data
    population_name <- "TestPop"

    sex_ratio <- 1.0
    starting_year <- 2009
    ending_year <- 2038

    newborns_df <- create_newborns_test_data(sex_ratio, starting_year, ending_year)
    size_df <- generate_populationsize_test_data()
    mortality_df <- generate_diseaseprevalences_test_data(incidence = TRUE)
    disability_df <- generate_diseaseprevalences_test_data(incidence = FALSE)

    result <- create_population_dir(
      population_name,
      sex_ratio,
      starting_year,
      newborns_df,
      size_df,
      mortality_df,
      disability_df
    )

    # Assert that the function returned TRUE
    expect_true(result)

    # Check that the directory was created
    expect_true(dir.exists(population_name))

    # Check that the expected files were created
    expect_true(file.exists(file.path(population_name, "newborns.xml")))
    expect_true(file.exists(file.path(population_name, "size.xml")))
    expect_true(file.exists(file.path(population_name, "overallmortality.xml")))
    expect_true(file.exists(file.path(population_name, "overalldisability.xml")))
  })
})
