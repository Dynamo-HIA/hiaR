test_that("get_file_tree returns correct structure", {
  withr::with_tempdir({
    dir.create("dir1")
    dir.create("dir2")
    dir.create("dir3")
    dir.create("dir4")

    dir.create("dir1/subdir1")
    file.create("dir1/file1.txt")
    file.create("dir2/file2.txt")
    dir.create("dir3/subdir2")
    file.create("dir3/subdir2/file3.txt")

    result <- get_file_tree(".")

    expected <- list( # list.files orders items alphabetically
      dir1 = list(
        "file1.txt" = NULL,
        subdir1 = NULL
      ),
      dir2 = list(
        "file2.txt" = NULL
      ),
      dir3 = list(
        subdir2 = list(
          "file3.txt" = NULL
        )
      ),
      dir4 = NULL
    )

    expect_equal(result, expected)
  })
})


test_that("get_file_tree handles empty directory", {
  withr::with_tempdir({
    result <- get_file_tree(".")
    expected <- list()
    names(expected) <- character(0)
    expect_equal(result, expected)
  })
})

test_that("get_relative_risk_source extracts sources correctly", {
  mock_data <- generate_test_file_groups(file_group = "disease")

  result <- get_relative_risk_source(mock_data[["inputs"]], mock_data[["lookup_items"]])

  expect_type(result, "list")
  expect_named(result, c("Disease1", "Disease2"))

  expect_equal(
    result$Disease1$Relative_Risks_From_Risk_Factor,
    data.frame(
      from = c("Risk1", "Risk2"),
      filename = c("RR_to_Disease1-Risk1.xml", "RR_to_Disease1-Risk2.xml")
    )
  )

  expect_equal(
    result$Disease1$Relative_Risks_From_Diseases,
    data.frame(
      from = c("Disease2"),
      filename = c("RR_to_Disease1-Disease2.xml")
    )
  )

  expect_equal(
    result$Disease2$Relative_Risks_From_Risk_Factor,
    data.frame(
      from = c("Risk3"),
      filename = c("RR_to_Disease2-Risk3.xml")
    )
  )
})

test_that("get_relative_risk_source returns item names when extract is FALSE", {
  mock_data <- generate_test_file_groups(file_group = "risk_factor")
  result <- get_relative_risk_source(mock_data[["inputs"]], mock_data[["lookup_items"]], extract = FALSE)

  expect_type(result, "list")
  expect_named(result, c("Factor1", "Factor2")) # TODO: replace with names(mock_data[["inputs"]])?

  expect_equal(
    result$Factor1$Relative_Risks_For_Death,
    data.frame(
      from = c("Factor1", "Factor1"),
      filename = c("some_file.xml", "another_file.xml")
    )
  )

  expect_equal(
    result$Factor2$Relative_Risks_For_Death,
    data.frame(
      from = c("Factor2", "Factor2"),
      filename = c("file2.xml", "file4.xml")
    )
  )

  expect_equal(
    result$Factor1$Relative_Risks_For_Disability,
    NULL
  )

  expect_equal(
    result$Factor2$Relative_Risks_For_Disability,
    data.frame(
      from = c("Factor2"),
      filename = c("file10.xml")
    )
  )
})

test_that("get_relative_risk_source handles empty input correctly", {
  empty_input <- list()
  result <- get_relative_risk_source(empty_input, c("Relative_Risks_From_Risk_Factor"))

  expect_equal(result, list())
})

test_that("get_relative_risk_source handles non-existent lookup items", {
  lookup_items <- c("Non_Existent_Item")
  mock_data <- generate_test_file_groups(file_group = "disease")
  result <- get_relative_risk_source(mock_data[["inputs"]], lookup_items)

  expect_type(result, "list")
  expect_named(result, c("Disease1", "Disease2"))
  expect_true(all(sapply(result, function(x) length(x) == 1)))
  expect_true(all(sapply(result, function(x) length(x[[1]]) == 0)))
})

test_that("get_relative_risk_source throws error for invalid input", {
  expect_error(get_relative_risk_source("not a list", c("Item")), "not TRUE")
  expect_error(get_relative_risk_source(list(), 3), "not TRUE")
  expect_error(get_relative_risk_source(list(), c("Item"), extract = "not a logical"), "not TRUE")
})

test_that("collect_relative_risks works on correct input", {
  data <- generate_relative_risk_data()
  actual <- collect_relative_risks(
    in_list = data[["input_list"]],
    keyword_map = data[["kw_map"]]
  )
  expected <- data[["expected"]]

  expect_equal(names(actual), names(expected))
  expect_type(actual, "list")
  expect_equal(
    actual$key_a[c("to", "filename", "from")],
    expected$key_a[c("to", "filename", "from")]
  )
  expect_equal(
    actual$key_b[c("to", "filename", "from")],
    expected$key_b[c("to", "filename", "from")]
  )
})

test_that("load_simulation_results validates input correctly", {
  expect_error(load_simulation_results("test", ""))

  tempdir <- withr::local_tempdir()

  expect_error(load_simulation_results(tempdir, ""))
})
