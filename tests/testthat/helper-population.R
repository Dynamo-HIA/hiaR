create_newborns_test_data <- function(sex_ratio = 1.0, starting_year = 2009, ending_year = 2038) {
  years <- starting_year:ending_year
  numbers <- sample(1:1000, length(years), replace = TRUE)

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
    number = sample(1:1000, n, replace = TRUE)
  )

  return(df)
}
