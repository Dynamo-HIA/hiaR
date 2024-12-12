

test_that("single_relative_risk_server works correctly", {
  # Mock choices
  mock_choices <- reactiveVal(list(
    "Choice 1" = "1",
    "Choice 2" = "2",
    "Choice 3" = "3"
  ))

  testServer(single_relative_risk_server, args = list(choices = mock_choices), {

    expect_equal(session$returned(), list())

    session$setInputs(check_group = c("1", "3"))
    expect_equal(session$returned(), c("Choice 1", "Choice 3"))

    # Test with all selections
    session$setInputs(check_group = c("1", "2", "3"))
    expect_equal(session$returned(), c("Choice 1", "Choice 2", "Choice 3"))

    # Test with null input
    session$setInputs(check_group = NULL)
    expect_equal(session$returned(), list())

    # Test reactivity
    mock_choices(list(
      "New Choice 1" = "1",
      "New Choice 2" = "2"
    ))
    session$setInputs(check_group = c("1", "2"))
    expect_equal(session$returned(), c("New Choice 1", "New Choice 2"))
  })
})
