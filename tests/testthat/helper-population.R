create_newborns_test_data <- function(sex_ratio = 1.0, starting_year = 2009, ending_year = 2038) {
  years <- starting_year:ending_year
  numbers <- runif(length(years), min = 0, max = 1000)

  # Combine into a data.frame
  df <- data.frame(
    year = years,
    number = numbers
  )

  return(df)
}


generate_populationsize_test_data <- function() {
  n <- 192

  df <- data.frame(
    age = sample(0:95, n, replace = TRUE),
    sex = sample(0:1, n, replace = TRUE),
    numbers = runif(n, min = 0, max = 1000)
  )

  return(df)
}
