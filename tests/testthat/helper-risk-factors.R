generate_transitionmatrix_test_data <- function(num_cat = 3) {
  df <- generate_age_sex_test_data()

  from <- 1:num_cat
  to <- 1:num_cat

  df <- merge(df, data.frame(from = from, to = to), by = NULL)

  df$percent <- 100/num_cat

  return(df)
}


generate_transitiondrift_test_data <- function() {
  df <- generate_age_sex_test_data()
  df$mean <- runif(nrow(df))

  return(df)
}



generate_riskfactorprevalences_test_data <- function(mode = c("continous", "categorical", "duration"), num_cat = 3) {
  df <- generate_age_sex_test_data()

  n <- nrow(df)

  cat <- 1:num_cat

  if (mode == "categorical") {
    df <- merge(df, data.frame(cat = cat))
    n <- nrow(df)
    df$percent <- 100/num_cat
  } else if (mode == "duration") {
    df <- merge(df, data.frame(duration = cat))
    n <- nrow(df)
    df$percent <- 100/num_cat
  } else {
    df$mean <- runif(n)
    df$standarddeviation <- runif(n)
    df$skewness <- runif(n)
  }

  return(df)
}


generate_riskfactorconfiguration_test_data <- function(mode = c("continuous", "categorical", "compound"), num_cat = 3) {
  if (mode == "continuous") {
    num_cat <- 9
  }

  flexdex <- 1:num_cat
  value <- runif(num_cat)
  name <- "test"

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
      referenceclass = sample(1:num_cat, 1)
    )
  } else {
    df <- data.frame(
      flexdex = flexdex,
      name = name
    )

    reference <- list(
      referenceclass = sample(1:num_cat, 1),
      durationclass = sample(1:num_cat, 1)
    )
  }

  return(list(data = df, reference = reference, type = mode))
}
