test_that("single_disease_server functions correctly", {
  testServer(
    single_disease_server, {
      session$setInputs(
        check = FALSE
      )
      expect_equal(session$returned(), NULL)

      session$setInputs(
        prevalence = "prev.xml",
        incidence = "inc.xml",
        excess_mortality = "exc_mort.xml",
        disability = "disability.xml"
      )
      expect_equal(session$returned(), NULL)

      session$setInputs(
        check = TRUE
      )
      actual <- session$returned()
      expected <- list(
        prevalence = "prev.xml",
        incidence = "inc.xml",
        excess_mortality = "exc_mort.xml",
        disability = "disability.xml"
      )
      expect_equal(actual, expected)

      session$setInputs(
        incidence = "inc2.xml"
      )

      actual <- session$returned()
      expected <- list(
        prevalence = "prev.xml",
        incidence = "inc2.xml",
        excess_mortality = "exc_mort.xml",
        disability = "disability.xml"
      )
      expect_equal(actual, expected)

    }

  )
})

test_reference_data <- list(
  diseases = list(
    "Disease A" = list(
      Prevalences = list("Low" = "low", "High" = "high"),
      Incidences = list("Rare" = "rare", "Frequent" = "frequent"),
      Excess_Mortalities = list("Minimal" = "minimal", "Severe" = "severe"),
      Disability = list("Low Impact" = "low", "High Impact" = "high")
    ),
    "Disease B" = list(
      Prevalences = list("Very Low" = "very_low", "Very High" = "very_high"),
      Incidences = list("Occasional" = "occasional", "Common" = "common"),
      Excess_Mortalities = list("Negligible" = "negligible", "Critical" = "critical"),
      Disability = list("Mild" = "mild", "Severe" = "severe")
    )
  ),
  populations = NULL,
  risk_factors = NULL,
  relative_risks = NULL
)

test_that("single_disease_server functions correctly", {
  testServer(
    disease_selection_server,
    args = list(reference_data = reactiveVal(NULL)), {
      expect_equal(session$returned(), list())
    }
  )

})










