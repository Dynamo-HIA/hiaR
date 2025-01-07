test_that("fetch_server_data functions correctly", {
  # Setup mock servers
  mock_server1 <- function() list(prevalence = "prev1", transitions = "trans1")
  mock_server2 <- function() NULL
  mock_server3 <- function() list(prevalence = "prev3", transitions = "trans3")

  server_list <- list(
    risk_factor_1 = mock_server1,
    risk_factor_2 = mock_server2,
    risk_factor_3 = mock_server3
  )

  item_names <- c("Factor1", "Factor2", "Factor3")

  # Test case 1: Normal operation
  result <- fetch_server_data("risk_factor_", server_list, item_names)

  expect_type(result, "list")
  expect_named(result, c("Factor1", "Factor3"))
  expect_length(result, 2)
  expect_equal(result$Factor1, list(prevalence = "prev1", transitions = "trans1"))
  expect_equal(result$Factor3, list(prevalence = "prev3", transitions = "trans3"))

  # Test case 2: All NULL servers
  all_null_servers <- list(
    risk_factor_1 = function() NULL,
    risk_factor_2 = function() NULL
  )

  result_all_null <- fetch_server_data("risk_factor_", all_null_servers, c("Factor1", "Factor2"))

  expect_type(result_all_null, "list")
  expect_length(result_all_null, 0)

  # Test case 3: Mismatched lengths
  expect_error(
    fetch_server_data("risk_factor_", server_list, item_names[1:2]),
    "length\\(server_list\\) == length\\(item_names\\) is not TRUE"
  )

  # Test case 4: Empty inputs
  expect_error(
    fetch_server_data("risk_factor_", list(), character(0)),
    NA
  )
})

test_that("create_choices_from_relative_risks has the right return values", {
  relative_risks <- generate_filtered_relative_risk_data()

  output <- create_choices_from_relative_risks(relative_risks)

  expect_length(output, 4)
  expect_type(output, "list")
  expected_names <- c(
    "from_diseases", "from_risk_factors",
    "to_death", "to_disability"
  )
  expect_equal(names(output), expected_names)

  lapply(seq_along(output), function(i) {
    expect_length(output[[i]], nrow(relative_risks[[i]]))
  })
})

test_that("create_choice_list works correctly", {
  rr_from_diseases <- generate_filtered_relative_risk_data()$from_diseases
  output <- create_choice_list(rr_from_diseases)

  expect_type(output, "list")
  expect_length(output, nrow(rr_from_diseases))

  expected <- list(
    "disease1 -> disease4" = 1,
    "disease1 -> disease3" = 2,
    "disease2 -> disease1" = 3
  )
  expect_equal(output, expected)

  empty_output <- create_choice_list(data.frame())
  expect_equal(empty_output, list())

  expect_error(
    create_choice_list("a"),
    "is.data.frame\\(input_df\\) is not TRUE"
  )

  output <- create_choice_list(rr_from_diseases, caller = "server")
  expected <- list(
    "RR_to_disease4-disease1.xml" = 1,
    "RR_to_disease3-disease1.xml" = 2,
    "RR_to_disease1-disease2.xml" = 3
  )
  expect_equal(output, expected)
})


test_that("filter_df_from_server_data works correctly", {
  mock_server_list <- list(
    a = function() {
      c("RR_to_disease4-disease1.xml")
    },
    b = function() {
      c("RR_to_disease1-risk_factor1.xml", "RR_to_disease2-risk_factor2.xml")
    }
  )
  relative_risks <- do.call(rbind, generate_filtered_relative_risk_data())
  rownames(relative_risks) <- NULL
  result <- filter_df_from_server_data(relative_risks, mock_server_list)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("from", "filename", "to"))
  expected_from_servers <- lapply(mock_server_list, function(x) x()) |> unlist()
  expect_true(all(expected_from_servers %in% result$filename))

  empty_relative_risks <- data.frame()
  empty_result <- filter_df_from_server_data(empty_relative_risks, mock_server_list)
  expect_equal(empty_result, data.frame())

  empty_mock_server_list <- list(
    a = function() {
      NULL
    },
    b = function() {
      NULL
    }
  )
  empty_result <- filter_df_from_server_data(relative_risks, empty_mock_server_list)
  expect_equal(empty_result, data.frame())
})

test_that("filter_relative_risks works correctly", {
  relative_risks <- generate_raw_relative_risk_data()
  diseases <- c("disease2")
  risk_factors <- c("risk_factor1", "risk_factor2")
  output <- filter_relative_risks(relative_risks, diseases, risk_factors)

  expect_length(output, 4)
  expect_named(output, c("from_diseases", "from_risk_factors", "to_death", "to_disability"))
  lapply(output, function(x) {
    expect_true(is.data.frame(x))
  })

  expect_equal(output$from_diseases$from, c("disease1", "disease2", "disease2"))
  expect_equal(output$from_risk_factors$from, c("risk_factor1", "risk_factor2"))
  expect_equal(output$to_death$from, c("risk_factor1", "risk_factor2"))
  expect_equal(output$to_disability$from, c("risk_factor1", "risk_factor2"))
})

test_that("filter_relative_risks handles NULL inputs", {
  relative_risks <- generate_raw_relative_risk_data()
  relative_risks$risk_factors$Relative_Risks_For_Death <- NULL
  diseases <- c("disease2")
  risk_factors <- c("risk_factor1", "risk_factor2")
  output <- filter_relative_risks(relative_risks, diseases, risk_factors)

  expect_equal(output$to_death, data.frame())
})

test_that("filter_relative_risks handles wrong arguments correctly", {
  relative_risks <- generate_raw_relative_risk_data()
  output <- filter_relative_risks(relative_risks, list(), list())
  expect_equal(output, list())
})

test_that("filter_items returns correct types", {
  current_choices <- generate_dummy_prevalence_and_transition_data()
  selected_risk_factors <- generate_dummy_selected_risk_factors()

  user_input <- selected_risk_factors$Alcohol_cat5$prevalence[[1]]
  output <- filter_items(
    user_input, selected_risk_factors, "prevalence", current_choices$transitions, FALSE
  )
  expect_type(output, "list")
  expect_type(output$default_value, "character")
  expect_type(output$keep_items, "list")
  expect_in(names(current_choices$transitions), names(output$keep_items))
})


test_that("filter_items returns correct values", {
  current_choices <- generate_dummy_prevalence_and_transition_data()
  selected_risk_factors <- generate_dummy_selected_risk_factors()

  # test for prevalences
  user_input <- selected_risk_factors$Alcohol_cat5$prevalence[[1]]
  output <- filter_items(
    user_input, selected_risk_factors, "prevalence", current_choices$transitions, FALSE
  )
  expected_missing <- lapply(selected_risk_factors, function(x) x[["transitions"]]) |>
    unlist()
  expect_true(all(!(expected_missing %in% unlist(output$keep_items))))
  expect_true(all(!(output$default_value %in% expected_missing)))


  # test for transitions
  user_input <- selected_risk_factors$Alcohol_cat5$transitions[[1]]
  output <- filter_items(
    user_input, selected_risk_factors, "transitions", current_choices$prevalences, FALSE
  )
  expected_missing <- lapply(selected_risk_factors, function(x) x[["prevalence"]]) |>
    unlist()
  expect_true(all(!(expected_missing %in% unlist(output$keep_items))))
  expect_true(all(!(output$default_value %in% expected_missing)))

  # test that full choices are returned when user input does not match with selected risk factors
  user_input <- current_choices$transitions$Smoking_dur[[1]]
  output <- filter_items(
    user_input, selected_risk_factors, "transitions", current_choices$prevalences, FALSE
  )
  expect_in(unlist(current_choices$prevalences), unlist(output$keep_items))
})


