generate_transitionmatrix_test_data <- function(n = 576) {
  if (n < 576 || n > 5760000) {
    stop("The number of rows must be between 576 and 5760000.")
  }

  # Generate random values for the data.frame according to the schema
  age <- sample(0:95, n, replace = TRUE)       # Age: Integer between 0 and 95
  sex <- sample(0:1, n, replace = TRUE)         # Sex: 0 or 1
  from <- sample(1:50, n, replace = TRUE)       # From state: Integer (arbitrary range 1-50)
  to <- sample(1:50, n, replace = TRUE)         # To state: Integer (arbitrary range 1-50)
  percent <- runif(n, 0, 100)                   # Percent: Random float between 0 and 100

  # Create the data.frame
  transition_matrix_df <- data.frame(
    age = age,
    sex = sex,
    from = from,
    to = to,
    percent = percent
  )

  return(transition_matrix_df)
}


generate_transitiondrift_test_data <- function() {
  n <- 192

  # Generate random values for the data.frame according to the schema
  age <- sample(0:95, n, replace = TRUE)
  sex <- sample(0:1, n, replace = TRUE)
  mean <- runif(n)

  # Create the data.frame
  transition_matrix_df <- data.frame(
    age = age,
    sex = sex,
    mean = mean
  )

  return(transition_matrix_df)
}



generate_riskfactorprevalences_test_data <- function(mode = c("continous", "categorical", "duration"), n = 192) {
  # Generate age values (0-95)
  age <- sample(0:95, n, replace = TRUE)

  # Generate sex values (0 or 1)
  sex <- sample(0:1, n, replace = TRUE)

  cat <- sample(1:50, n, replace = TRUE)

  duration <- sample(0:20, n, replace = TRUE)

  mean <- runif(n)
  std <- runif(n)
  skewness <- runif(n)

  # Generate percent values (0-100)
  percent <- runif(n, min = 0, max = 100)

  # Create the data frame
  df <- data.frame(
    age = age,
    sex = sex
  )

  if (mode == "categorical") {
    df$cat <- cat
    df$percent <- percent
  } else if (mode == "duration") {
    df$duration <- duration
    df$percent <- percent
  } else {
    df$mean <- mean
    df$standarddeviation <- std
    df$skewness <- skewness
  }

  return(df)
}


generate_riskfactorconfiguration_test_data <- function(mode = c("continuous", "categorical", "compound")) {
  # Generate age values (0-95)
  n <- switch(mode,
    continuous = 9,
    categorical = 50,
    compound = 10
  )

  flexdex <- 1:n
  value <- runif(n)
  name <- "test"

  reference <- list(
    referencevalue = runif(1),
    referenceclass = sample(1:n, 1),
    referenceduration = sample(1:n, 1)
  )

  if (mode == "continuous") {
    df <- data.frame(
      flexdex = flexdex,
      value = value
    )

    reference <- list(
      referencevalue = runif(1)
    )
  } else if (mode == "categorical") {
    df <- data.frame(
      flexdex = flexdex,
      name = name
    )

    reference <- list(
      referenceclass = sample(1:n, 1)
    )
  } else {
    df <- data.frame(
      flexdex = flexdex,
      name = name
    )

    reference <- list(
      referenceclass = sample(1:n, 1),
      referenceduration = sample(1:n, 1)
    )
  }

  return(list(data = df, reference = reference, type = mode))
}
