test_that("create_simulation_dir creates directory and configuration file correctly", {
  withr::with_tempdir({
    simulation_name <- "test_simulation"

    # Test execution
    result <- create_simulation_dir(
      simulation_name,
      has_newborns = FALSE,
      starting_year = 2024,
      number_of_years = 5,
      population_size = 1000,
      min_age = 18,
      max_age = 65,
      time_step = 1,
      ref_scenario_name = "test_reference",
      random_seed = 42,
      result_type = "standard",
      population_name = "test_population",
      scenarios = list(
        list(
          uniquename = "scenario1",
          successRate = 75,
          targetMinAge = 20,
          targetMaxAge = 60,
          targetSex = 1,
          transfilename = "trans1.csv",
          prevfilename = "prev1.csv"
        )
      ),
      diseases = list(
        list(
          uniquename = "disease1",
          prevfilename = "disease_prev.csv",
          incfilename = "disease_inc.csv",
          excessmortfilename = "mort.csv",
          dalyweightsfilename = "daly.csv"
        )
      ),
      risk_factors = list(
        uniquename = "risk1",
        transfilename = "risk_trans.csv",
        prevfilename = "risk_prev.csv"
      ),
      relative_risks = list(
        list(
          RRindex = 1,
          isRRfrom = "disease1",
          isRRto = "risk1",
          isRRFile = "rr.csv"
        )
      ))

    # Assertions
    expect_true(result)
    expect_true(dir.exists(simulation_name))
    expect_true(file.exists(file.path(simulation_name, "configuration.xml")))
  })
})
