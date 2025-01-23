test_that("single_risk_factor_server functions correctly", {
  testServer(
    single_risk_factor_server,
    {
      session$setInputs(
        prevalence = "prev.xml",
        transitions = "transitions.xml"
      )
      actual <- session$returned()
      expected <- list(
        prevalence = "prev.xml",
        transitions = "transitions.xml"
      )
      expect_equal(actual, expected)

      session$setInputs(
        transitions = "transitions2.xml"
      )

      actual <- session$returned()
      expected <- list(
        prevalence = "prev.xml",
        transitions = "transitions2.xml"
      )

      expect_equal(actual, expected)
    }
  )
})
