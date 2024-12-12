test_that("program config returns the right type with the right names", {
  testServer(program_config_server, {
    session$flushReact()
    returned <- session$returned
    expected_names <- c("dynamo_path", "working_path", "reference_data")

    expect_type(returned, "list")
    expect_true(setequal(names(returned), expected_names))
  })
})


# Mock the get_reference_data function
mock_get_reference_data <- function(path) {
  list(diseases = c("Disease1", "Disease2"))
}

test_that("program_config_server returns correct and reactive values", {
  testServer(program_config_server, {
    local_mocked_bindings(
      parse_dirpath_wrapper = function(...) "/path/to/working/dir",
      parse_filepath_wrapper = function(...) {
        data.frame(
          name = "myapplication.exe",
          size = 100,
          type = "",
          datapath = "/path/to/app/myapplication.exe"
        )
      },
      get_reference_data = mock_get_reference_data
    )

    # Test working directory selection
    session$setInputs(working_button = TRUE) # pass anything non-NULL
    expect_equal(working_path(), "/path/to/working/dir")
    expect_equal(reference_data(), list(diseases = c("Disease1", "Disease2")))

    # Test DYNAMO executable selection
    session$setInputs(dynamo_button = TRUE) # pass anything non-NULL
    expect_equal(dynamo_path(), "/path/to/app/myapplication.exe")

    # Test that reactive values are returned
    result <- session$getReturned()
    expect_type(result, "list")
    expect_named(result, c("working_path", "dynamo_path", "reference_data"))
    expect_type(result$working_path, "closure")
    expect_type(result$dynamo_path, "closure")
    expect_type(result$reference_data, "closure")
  })
})
