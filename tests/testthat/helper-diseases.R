generate_relriskfromriskfactor_test_data <- function(mode = "continuous", num_cat = 3) {
  # Ensure mode is valid
  mode <- match.arg(mode, c("continuous", "compound", "categorical"))

  df <- generate_age_sex_test_data()

  n <- nrow(df)

  cat <- 1:num_cat

  if (mode == "continuous") {
    df$value <- runif(n)
  } else if (mode == "compound") {
    df <- merge(df, data.frame(cat = cat), by = NULL)

    n <- nrow(df)

    # df$value <- runif(n)
    df$begin <- runif(n)
    df$alfa <- runif(n)
    df$end <- runif(n)

  } else {  # categorical mode

    df <- merge(df, data.frame(cat = cat), by = NULL)

    n <- nrow(df)

    df$value <- runif(n)
  }

  return(df)
}


generate_relriskfromdisease_test_data <- function() {
  df <- generate_age_sex_test_data()

  # Generate random float values for relative risk
  df$value <- runif(nrow(df), min = 0.8, max = 1.5)

  return(df)
}


generate_diseaseprevalences_test_data <- function(incidence = FALSE) {
  df <- generate_age_sex_test_data()

  df$percent <- runif(nrow(df), min = 5, max = 10)

  if (incidence) {
    names(df)[3] <- "value"
  }

  return(df)
}


generate_excessmortality_test_data <- function(parameter_type = "acutely_fatal") {
  df <- generate_age_sex_test_data()

  n <- nrow(df)

  df$unit <- runif(n, min = 0.8, max = 1.5)

  if (parameter_type == "accutely_fatal") {
    df$acutelyfatal <- runif(n, min = 0.05, max = 0.1)  # Assuming a proportion between 0 and 1
    df$curedfraction <- 0
  } else {
    df$acutelyfatal <- 0
    df$curedfraction <- runif(n, min = 0.05, max = 0.1)  # Assuming a proportion between 0 and 1
  }

  return(df)
}
