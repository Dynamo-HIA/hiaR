generate_relriskfromriskfactor_test_data <- function(mode = "continuous", n = 192) {
  # Ensure mode is valid
  mode <- match.arg(mode, c("continuous", "compound", "categorical"))

  # Set n limits based on mode
  n <- switch(mode,
    "continuous" = 192,
    "compound" = max(192, min(n, 1920)),
    "categorical" = max(192, min(n, 9600))
  )

  # Generate common fields
  age <- sample(0:95, n, replace = TRUE)
  sex <- sample(0:1, n, replace = TRUE)

  if (mode == "continuous") {
    # Continuous mode: single value for each row
    value <- runif(n)
    df <- data.frame(age = age, sex = sex, value = value)
  } else if (mode == "compound") {
    # Compound mode: cat and choice between value or begin/alfa/end
    cat <- sample(1:10, n, replace = TRUE)
    use_value <- sample(c(TRUE, FALSE), n, replace = TRUE)

    value <- ifelse(use_value, runif(n), NA)
    begin <- ifelse(!use_value, runif(n), NA)
    alfa <- ifelse(!use_value, runif(n), NA)
    end <- ifelse(!use_value, runif(n), NA)

    df <- data.frame(
      age = age,
      sex = sex,
      cat = cat,
      value = value,
      begin = begin,
      alfa = alfa,
      end = end
    )
  } else {  # categorical mode
    # Categorical mode: cat (0-50) and value for each row
    cat <- sample(0:50, n, replace = TRUE)
    value <- runif(n)

    df <- data.frame(age = age, sex = sex, cat = cat, value = value)
  }

  return(df)
}


generate_relriskfromdisease_test_data <- function(n = 192) {
  n <- max(192, min(n, 1920))

  # Generate age values (0-95)
  age <- sample(0:95, n, replace = TRUE)

  # Generate sex values (0 or 1)
  sex <- sample(0:1, n, replace = TRUE)

  # Generate random float values for relative risk
  value <- runif(n, min = 0, max = 10)  # Assuming relative risk between 0 and 10

  # Create the data frame
  df <- data.frame(
    age = age,
    sex = sex,
    value = value
  )

  return(df)
}


generate_diseaseprevalences_test_data <- function(incidence = FALSE) {
  # Fixed number of rows (192)
  n <- 192

  # Generate age values (0-95)
  age <- sample(0:95, n, replace = TRUE)

  # Generate sex values (0 or 1)
  sex <- sample(0:1, n, replace = TRUE)

  # Generate percent values (0-100)
  percent <- runif(n, min = 0, max = 100)

  # Create the data frame
  df <- data.frame(
    age = age,
    sex = sex,
    percent = percent
  )

  if (incidence) {
    names(df)[3] <- "value"
  }

  return(df)
}


generate_excessmortality_test_data <- function() {
  # Mortalities data
  n <- 192  # Fixed number of mortality entries

  df <- data.frame(
    age = sample(0:95, n, replace = TRUE),
    sex = sample(0:1, n, replace = TRUE),
    unit = runif(n, min = 0, max = 10),  # Assuming a range of 0-10 for relative risk/hazard ratio/odds ratio
    acutelyfatal = runif(n, min = 0, max = 1),  # Assuming a proportion between 0 and 1
    curedfraction = runif(n, min = 0, max = 1)  # Assuming a proportion between 0 and 1
  )

  return(df)
}
