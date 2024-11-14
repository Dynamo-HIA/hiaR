create_newborns_test_data <- function(sex_ratio = 1.0, starting_year = 2009, ending_year = 2038) {
  years <- starting_year:ending_year
  numbers <- sample(200:300, length(years), replace = TRUE)

  # Combine into a data.frame
  df <- data.frame(
    year = years,
    number = numbers
  )

  return(df)
}


generate_age_sex_test_data <- function(age = 0:95, sex = 0:1) {
  return(expand.grid(age = age, sex = sex))
}


generate_populationsize_test_data <- function() {
  df <- generate_age_sex_test_data()

  df$number <- sample(200:300, nrow(df), replace = TRUE)

  return(df)
}
