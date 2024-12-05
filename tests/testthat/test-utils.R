
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

