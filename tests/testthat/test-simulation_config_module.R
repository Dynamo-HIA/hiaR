
test_that("simulation config returns the right values", {

  testServer(simulation_config_server,
             args = list(reference_data = reactiveVal(NULL)), {
    session$setInputs(
      simulation_name = "my_simulation",
      population = "population1",
      newborns = "yes",
      starting_year = 2004,
      population_size = 100,
      years = 10,
      time_step = 1,
      random_seed = 4582
    )

    actual <- session$returned()
    expected <- list(
      simulation_name = "my_simulation",
      population = "population1",
      newborns = "yes",
      starting_year = 2004,
      population_size = 100,
      years = 10,
      time_step = 1,
      random_seed = 4582
    )
    expect_true(setequal(names(actual), names(expected)))
    lapply(names(actual), function(x) {
      x_expected <- expected[[x]]
      x_actual <- actual[[x]]
      expect_equal(x_expected, x_actual)
    })

  })
})




