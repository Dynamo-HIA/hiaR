# Mock data creation function for testing (same as previous test file)
create_mock_dataframe <- function(
  years = c(2020, 2021, 2022),
  scenarios = c(0, 1, 2),
  ages = seq(0, 95, by = 1),
  genders = c("males", "females")
) {
  # Generate all combinations
  df <- expand.grid(
    year = years,
    scenario = scenarios,
    age = ages,
    gender = genders,
    stringsAsFactors = FALSE
  )

  # Set seed for reproducibility
  set.seed(123)

  # Add synthetic data columns
  df$total.number <- stats::rnorm(
    nrow(df),
    mean = 5000,
    sd = 1000
  )

  # Disability prevalence - varying by age and scenario
  df$with.disability <- abs(stats::rnorm(
    nrow(df),
    mean = 500 * (1 - abs(df$age - 50) / 100) * (1 + df$scenario * 0.2),
    sd = 100
  ))

  # Disease prevalence - varying by age, scenario, and gender
  df$with.disease <- abs(stats::rnorm(
    nrow(df),
    mean = 200 * (1 - abs(df$age - 60) / 100) *
           (1 + df$scenario * 0.15) *
           ifelse(df$gender == "males", 1.2, 0.8),
    sd = 50
  ))

  # Life expectancy calculations
  df$total.life.expectancy <- stats::rnorm(
    nrow(df),
    mean = 80 - abs(df$age - 50) * 0.1,
    sd = 5
  )

  # Expectancy with disability
  df$expectancy.with.disability <- df$total.life.expectancy -
    abs(stats::rnorm(nrow(df), mean = 5 * df$scenario, sd = 2))

  # Expectancy with all diseases
  df$expectancy.with.all.diseases <- df$total.life.expectancy -
    abs(stats::rnorm(nrow(df), mean = 7 * df$scenario, sd = 3))

  # Specific disease expectancy (e.g., cancer)
  df$life.expectany.with.cancer <- df$total.life.expectancy -
    abs(stats::rnorm(nrow(df), mean = 6 * df$scenario, sd = 2.5))

  # Mortality rate
  df$mortality <- abs(stats::rnorm(
    nrow(df),
    mean = 0.05 * (1 + df$age / 100),
    sd = 0.01
  ))

  # Disease prevalence
  df$cancer <- abs(stats::rnorm(
    nrow(df),
    mean = 0.03 * (1 + df$age / 100) *
           (1 + df$scenario * 0.1) *
           ifelse(df$gender == "males", 1.2, 0.8),
    sd = 0.005
  ))

  # Incidence of cancer
  df$incidence.of.cancer <- abs(stats::rnorm(
    nrow(df),
    mean = 0.01 * (1 + df$age / 100) *
           (1 + df$scenario * 0.1) *
           ifelse(df$gender == "males", 1.2, 0.8),
    sd = 0.002
  ))

  # Risk classes
  df$riskClass <- sample(
    c("low", "medium", "high"),
    nrow(df),
    replace = TRUE,
    prob = c(0.5, 0.3, 0.2)
  )

  return(df)
}

# Snapshot tests for population_pyramid
test_that("plot_population_pyramid snapshots", {
  # Prepare mock data with consistent seed for reproducibility
  set.seed(123)
  mock_df <- create_mock_dataframe()
  mock_df$gender <- ifelse(mock_df$gender == "males", 0, 1)
  # Standard population pyramid
  vdiffr::expect_doppelganger("population_pyramid_standard",
                              plot_population_pyramid(mock_df, year = 2020, scenario = 0)
  )

  # Population pyramid with gender split
  vdiffr::expect_doppelganger("population_pyramid_gender_split",
                              plot_population_pyramid(mock_df, year = 2020, scenario = 0, split_gender = TRUE)
  )

  # Population pyramid with difference
  vdiffr::expect_doppelganger("population_pyramid_with_difference",
                              plot_population_pyramid(mock_df, year = 2020, scenario = 1, difference = TRUE)
  )

  # Population pyramid without total
  vdiffr::expect_doppelganger("population_pyramid_no_total",
                              plot_population_pyramid(mock_df, year = 2020, scenario = 0, show_total = FALSE)
  )
})

# Snapshot tests for life_expectancy
test_that("plot_life_expectancy snapshots", {
  # Prepare mock data with consistent seed for reproducibility
  set.seed(123)
  mock_df <- create_mock_dataframe()
  # Standard life expectancy plot
  vdiffr::expect_doppelganger("life_expectancy_standard",
                              plot_life_expectancy(mock_df, age = 0)
  )

  # Life expectancy plot with specific disease
  vdiffr::expect_doppelganger("life_expectancy_specific_disease",
                              plot_life_expectancy(mock_df, age = 0, disease = "cancer")
  )

  # Life expectancy plot without disability
  vdiffr::expect_doppelganger("life_expectancy_no_disability",
                              plot_life_expectancy(mock_df, age = 0, show_disability = FALSE)
  )

  # Life expectancy plot without disease
  vdiffr::expect_doppelganger("life_expectancy_no_disease",
                              plot_life_expectancy(mock_df, age = 0, show_disease = FALSE)
  )
})

# Snapshot tests for time series plots
test_that("plot_time_series snapshots", {
  # Prepare mock data with consistent seed for reproducibility
  set.seed(123)
  mock_df <- create_mock_dataframe()
  # Time series with year as x-axis
  vdiffr::expect_doppelganger("time_series_year_axis",
                              plot_time_series(mock_df, x_axis = "year", y_axis = "mortality", age = 0)
  )

  # Time series with age as x-axis
  vdiffr::expect_doppelganger("time_series_age_axis",
                              plot_time_series(mock_df, x_axis = "age", y_axis = "mortality", year = 2020)
  )
})

# Snapshot tests for mortality plot
test_that("plot_mortality snapshots", {
  # Prepare mock data with consistent seed for reproducibility
  set.seed(123)
  mock_df <- create_mock_dataframe()
  # Standard mortality plot
  vdiffr::expect_doppelganger("mortality_plot_standard",
                              plot_mortality(mock_df, age = 0)
  )

  # Mortality plot with different x-axis
  vdiffr::expect_doppelganger("mortality_plot_age_axis",
                              plot_mortality(mock_df, x_axis = "age", year = 2020)
  )
})

# Snapshot tests for prevalence plot
test_that("plot_prevalence snapshots", {
  # Prepare mock data with consistent seed for reproducibility
  set.seed(123)
  mock_df <- create_mock_dataframe()
  # Standard prevalence plot
  vdiffr::expect_doppelganger("prevalence_plot_cancer",
                              plot_prevalence(mock_df, disease = "cancer", age = 0)
  )

  # Prevalence plot with different x-axis
  vdiffr::expect_doppelganger("prevalence_plot_age_axis",
                              plot_prevalence(mock_df, disease = "cancer", x_axis = "age", year = 2020)
  )
})

# Snapshot tests for incidence plot
test_that("plot_incidence snapshots", {
  # Prepare mock data with consistent seed for reproducibility
  set.seed(123)
  mock_df <- create_mock_dataframe()
  # Standard incidence plot
  vdiffr::expect_doppelganger("incidence_plot_cancer",
                              plot_incidence(mock_df, disease = "cancer", age = 0)
  )

  # Incidence plot with different x-axis
  vdiffr::expect_doppelganger("incidence_plot_age_axis",
                              plot_incidence(mock_df, disease = "cancer", x_axis = "age", year = 2020)
  )
})

# Snapshot tests for risk factor plot
test_that("plot_risk_factor snapshots", {
  # Prepare mock data with consistent seed for reproducibility
  set.seed(123)
  mock_df <- create_mock_dataframe()
  # Standard risk factor plot
  vdiffr::expect_doppelganger("risk_factor_plot_low",
                              plot_risk_factor(mock_df, risk_factor_class = "low", age = 0)
  )

  # Risk factor plot with different x-axis
  vdiffr::expect_doppelganger("risk_factor_plot_age_axis",
                              plot_risk_factor(mock_df, risk_factor_class = "low", x_axis = "age", year = 2020)
  )
})
