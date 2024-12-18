prepare_plot_data <- function(df, year, scenario, age = 0,
                               split_gender = TRUE,
                               difference = FALSE) {
  # Filter data for specific year and scenario
  df_filtered <- df[df$year == year & df$scenario == scenario, ]

  # If a specific age is provided, filter for that age
  if (age > 0) {
    df_filtered <- df_filtered[df_filtered$age == age, ]
  }

  # Handle gender splitting
  if (split_gender) {
    df_filtered$total.number <- ifelse(df_filtered$gender == 0,
                                       -df_filtered$total.number,
                                       df_filtered$total.number)
    df_filtered$with.disability <- ifelse(df_filtered$gender == 0,
                                          -df_filtered$with.disability,
                                          df_filtered$with.disability)
    df_filtered$with.disease <- ifelse(df_filtered$gender == 0,
                                       -df_filtered$with.disease,
                                       df_filtered$with.disease)
  }

  # Handle reference data for difference calculation
  if (difference) {
    ref_data <- df[df$year == year & df$scenario == 0, ]

    if (split_gender) {
      return(list(
        plot_data = df_filtered,
        ref_disability = ifelse(df_filtered$gender == 0, -ref_data$with.disability, ref_data$with.disability),
        ref_disease = ifelse(df_filtered$gender == 0, -ref_data$with.disease, ref_data$with.disease)
      ))
    } else {
      return(list(
        plot_data = df_filtered,
        ref_disability = ref_data$with.disability,
        ref_disease = ref_data$with.disease
      ))
    }
  }

  return(df_filtered)
}

validate_inputs <- function(df,
                             year = NULL,
                             scenario = NULL,
                             age = NULL,
                             disease = NULL,
                             risk_factor_class = NULL) {
  # Check if df is a data frame
  stopifnot(is.data.frame(df))

  # Check required columns
  required_cols <- c("year", "scenario", "age", "gender")
  missing_cols <- setdiff(required_cols, names(df))
  stopifnot(length(missing_cols) == 0)

  # Validate year if provided
  if (!is.null(year)) {
    stopifnot(
      is.numeric(year),
      length(year) == 1,
      year >= 0,
      year %in% unique(df$year)
    )
  }

  # Validate scenario if provided
  if (!is.null(scenario)) {
    stopifnot(
      is.numeric(scenario),
      length(scenario) == 1,
      scenario %in% unique(df$scenario)
    )
  }

  # Validate age if provided
  if (!is.null(age)) {
    stopifnot(
      is.numeric(age),
      length(age) == 1,
      age >= 0,
      age %in% unique(df$age)
    )
  }

  # Validate disease if provided
  if (!is.null(disease)) {
    stopifnot(
      is.character(disease),
      length(disease) == 1
    )

    # Special handling for 'all_diseases'
    if (disease != "all_diseases") {
      # Check if disease column exists
      disease_col <- paste0("life.expectany.with.", disease)
      stopifnot(disease_col %in% names(df))
    }
  }

  # Validate risk factor class if provided
  if (!is.null(risk_factor_class)) {
    stopifnot(
      is.character(risk_factor_class),
      length(risk_factor_class) == 1,
      "riskClass" %in% names(df),
      risk_factor_class %in% unique(df$riskClass)
    )
  }
}

#' Create a Population Pyramid Visualization
#'
#' @param df A data frame containing batch output results from DYNAMO-HIA.
#' @param year The specific year to plot.
#' @param scenario The index of the scenario to plot.
#' @param difference Logical, whether to show differences from reference scenario (default` FALSE`).
#' @param split_gender Logical, whether to split data by gender (default `TRUE`).
#' @param show_total Logical, whether to display total population (default `TRUE`).
#' @param show_disability Logical, whether to display disability data (default `TRUE`).
#' @param show_disease Logical, whether to display disease data (default `TRUE`).
#' @param alpha Transparency level for plot elements (default 0.8).
#'
#' @return A `ggplot` object representing the population pyramid.
#' @export
plot_population_pyramid <- function(df,
                                    year,
                                    scenario,
                                    difference = FALSE,
                                    split_gender = TRUE,
                                    show_total = TRUE,
                                    show_disability = TRUE,
                                    show_disease = TRUE,
                                    alpha = 0.8) {
  # Validate inputs
  validate_inputs(df, year = year, scenario = scenario)

  # Validate alpha
  stopifnot(
    is.numeric(alpha),
    alpha >= 0,
    alpha <= 1
  )

  # Validate boolean parameters
  stopifnot(
    is.logical(difference),
    is.logical(split_gender),
    is.logical(show_total),
    is.logical(show_disability),
    is.logical(show_disease)
  )

  # Prepare plot data (rest of the function remains the same)
  if (difference) {
    plot_prep <- prepare_plot_data(df, year, scenario,
                                   split_gender = split_gender,
                                   difference = TRUE)
    plot_data <- plot_prep$plot_data
    ref_disability <- plot_prep$ref_disability
    ref_disease <- plot_prep$ref_disease
  } else {
    plot_data <- prepare_plot_data(df, year, scenario,
                                   split_gender = split_gender)
  }

  # Base plot
  p <- ggplot(plot_data, aes(y = age, group = gender)) +
    labs(
      title = "Population size across age groups",
      x = "Population size",
      y = "Age",
      fill = ""
    ) +
    scale_x_continuous(labels = abs) +
    scale_fill_viridis_d() +
    theme_classic()

  # Add plot layers conditionally
  if (show_total) {
    p <- p + geom_col(aes(x = total.number, fill = "Total"), orientation = "y")
  }

  if (show_disability) {
    p <- p +
      geom_col(
        aes(x = with.disability, fill = paste("With disability scenario", scenario)),
        orientation = "y",
        alpha = alpha
      )

    if (difference) {
      p <- p +
        geom_rect(
          aes(
            xmin = 0,
            xmax = ref_disability,
            ymin = age - 0.5,
            ymax = age + 0.5,
            fill = "With disability reference"
          ),
          alpha = alpha
        )
    }
  }

  if (show_disease) {
    p <- p +
      geom_col(
        aes(x = with.disease, fill = paste("With disease scenario", scenario)),
        orientation = "y",
        alpha = alpha
      )

    if (difference) {
      p <- p +
        geom_rect(
          aes(
            xmin = 0,
            xmax = ref_disease,
            ymin = age - 0.5,
            ymax = age + 0.5,
            fill = "With disease reference"
          ),
          alpha = alpha
        )
    }
  }

  # Add gender annotations if split
  if (split_gender) {
    total_range <- range(plot_data$total.number)
    p <- p +
      annotate("text",
               x = 0.9 * total_range,
               y = 0.9 * max(plot_data$age),
               label = c("Men", "Women"))
  }

  return(p)
}

#' Plot Life Expectancy Across Scenarios and Gender
#'
#' @param df A data frame containing life expectancy data from DYNAMO-HIA.
#' @param age Age group to analyze. When default (`NULL`), the youngest age group is selected.
#' @param disease Specific disease to analyze (default `"all_diseases"`).
#' @param show_disability Logical, whether to display disability impact (default `TRUE`)
#' @param show_disease Logical, whether to display disease impact (default `TRUE`)
#' @param alpha Transparency level for plot elements (default 0.8)
#'
#' @return A `ggplot` object representing life expectancy across scenarios.
#' @export
plot_life_expectancy <- function(df,
                                 age = NULL,
                                 disease = "all_diseases",
                                 show_disability = TRUE,
                                 show_disease = TRUE,
                                 alpha = 0.8) {
  if (is.null(age)) {
    age <- min(df$age)
  }

  # Validate inputs
  validate_inputs(df, age = age, disease = disease)

  # Validate alpha
  stopifnot(
    is.numeric(alpha),
    alpha >= 0,
    alpha <= 1
  )

  # Validate boolean parameters
  stopifnot(
    is.logical(show_disability),
    is.logical(show_disease)
  )

  # Rest of the original function remains the same
  # Prepare data
  df_filtered <- df[df$age == age, ]

  # Transform gender and calculate outcomes
  df_filtered$gender <- ifelse(df_filtered$gender == "males", "Men", "Women")

  if (disease == "all_diseases") {
    df_filtered$disease_outcome <- df_filtered$total.life.expectancy -
      df_filtered$expectancy.with.all.diseases
    disease_label <- "With all diseases"
  } else {
    disease_column <- paste0("life.expectany.with.", disease)
    df_filtered$disease_outcome <- df_filtered$total.life.expectancy -
      df_filtered[[disease_column]]
    disease_label <- paste("With", disease)
  }

  df_filtered$disability_outcome <- df_filtered$total.life.expectancy -
    df_filtered$expectancy.with.disability

  # Base plot
  p <- ggplot(df_filtered, aes(y = scenario)) +
    facet_grid(rows = vars(gender)) +
    geom_col(
      aes(x = total.life.expectancy, fill = "Total"),
      orientation = "y",
      alpha = alpha,
      position = "dodge"
    )

  # Conditionally add disease and disability layers
  if (show_disease) {
    p <- p +
      geom_col(
        aes(x = disease_outcome, fill = disease_label),
        orientation = "y",
        alpha = alpha,
        position = "dodge"
      )
  }

  if (show_disability) {
    p <- p +
      geom_col(
        aes(x = disability_outcome, fill = "With disability"),
        orientation = "y",
        alpha = alpha,
        position = "dodge"
      )
  }

  # Final plot styling
  p <- p +
    labs(
      title = paste("Life expectancy across scenarios and gender for age group", age),
      x = "Life expectancy (in years)",
      y = "Scenario",
      fill = ""
    ) +
    scale_fill_viridis_d() +
    theme_classic()

  return(p)
}

plot_time_series <- function(df,
                             x_axis = "year",
                             y_axis = "mortality",
                             split_by = "scenario",
                             age = NULL,
                             year = NULL) {
  if (is.null(age)) {
    age <- min(df$age)
  }

  if (is.null(year)) {
    year <- min(df$year)
  }

  # Validate inputs
  validate_inputs(df, age = age, year = year)

  # Validate x_axis, y_axis, and split_by
  stopifnot(
    is.character(x_axis),
    length(x_axis) == 1,
    is.character(y_axis),
    length(y_axis) == 1,
    is.character(split_by),
    length(split_by) == 1,
    x_axis %in% names(df),
    y_axis %in% names(df),
    split_by %in% names(df)
  )

  # Rest of the original function remains the same
  # Filter data based on specified conditions
  if (x_axis == "year") {
    df_filtered <- df[df$age == age, ]
    x_label <- "Year"
  } else {
    df_filtered <- df[df$year == year, ]
    x_label <- "Age (in years)"
  }

  # Convert scenario to character
  df_filtered$scenario <- as.character(df_filtered$scenario)

  # Create base plot
  ggplot(df_filtered, aes_string(x = x_axis, y = y_axis, fill = split_by)) +
    geom_col() +
    scale_fill_viridis_d(name = tools::toTitleCase(split_by)) +
    xlab(x_label) +
    theme_classic()
}

#' Plot Mortality Rates
#'
#' @param df A data frame containing batch output results from DYNAMO-HIA.
#' @param x_axis Column to use for x-axis (default `"year"`).
#' @param split_by Column to use for color/fill splitting (default `"scenario"`).
#' @param age Optional age group to filter. By default (`NULL`), the youngest age group is used.
#' @param year Optional year to filter. By default (`NULL`), the earliest year is used.
#'
#' @return A ggplot object representing mortality rates
#' @export
plot_mortality <- function(df,
                           x_axis = "year",
                           split_by = "scenario",
                           age = NULL,
                           year = NULL) {

  # Calculate mortality rate
  df$mortality <- df$mortality / df$total.number

  plot_time_series(
    df,
    x_axis = x_axis,
    y_axis = "mortality",
    split_by = split_by,
    age = age,
    year = year
  ) +
    labs(
      title = "Mortality rate",
      y = "Mortality rate"
    )
}

#' Plot Disease Prevalence Rates
#'
#' @param df A data frame containing batch output results from DYNAMO-HIA.
#' @param disease Disease to plot prevalence rates for.
#' @param x_axis Column to use for x-axis (default `"year"`).
#' @param split_by Column to use for color/fill splitting (default `"scenario"`).
#' @param age Optional age group to filter. By default (`NULL`), the youngest age group is used.
#' @param year Optional year to filter. By default (`NULL`), the earliest year is used.
#'
#' @return A ggplot object representing prevalence rates.
#' @export
plot_prevalence <- function(df,
                            disease,
                            x_axis = "year",
                            split_by = "scenario",
                            age = NULL,
                            year = NULL) {
  plot_time_series(
    df,
    x_axis = x_axis,
    y_axis = disease,
    split_by = split_by,
    age = age,
    year = year
  ) +
    labs(
      title = paste("Prevalence of disease", disease),
      y = "Prevalence"
    )
}

#' Plot Disease Incidence Rates
#'
#' @param df A data frame containing batch output results from DYNAMO-HIA.
#' @param disease Disease to plot prevalence rates for.
#' @param x_axis Column to use for x-axis (default `"year"`).
#' @param split_by Column to use for color/fill splitting (default `"scenario"`).
#' @param age Optional age group to filter. By default (`NULL`), the youngest age group is used.
#' @param year Optional year to filter. By default (`NULL`), the earliest year is used.
#'
#' @return A ggplot object representing incidence rates.
#' @export
plot_incidence <- function(df,
                           disease,
                           x_axis = "year",
                           split_by = "scenario",
                           age = NULL,
                           year = NULL) {
  plot_time_series(
    df,
    x_axis = x_axis,
    y_axis = paste0("incidence.of.", disease),
    split_by = split_by,
    age = age,
    year = year
  ) +
    labs(
      title = paste("Incidence of disease", disease),
      y = "Incidence"
    )
}

#' Plot Risk Factor Prevalence Rates
#'
#' @param df A data frame containing batch output results from DYNAMO-HIA.
#' @param risk_factor_class Class of the risk factor to plot.
#' @param x_axis Column to use for x-axis (default `"year"`).
#' @param split_by Column to use for color/fill splitting (default `"scenario"`).
#' @param age Optional age group to filter. By default (`NULL`), the youngest age group is used.
#' @param year Optional year to filter. By default (`NULL`), the earliest year is used.
#'
#' @return A ggplot object representing prevalence rates.
#' @export
plot_risk_factor <- function(df,
                             risk_factor_class,
                             x_axis = "year",
                             split_by = "scenario",
                             age = NULL,
                             year = NULL) {
  # Validate inputs
  validate_inputs(df,
                  age = age,
                  year = year,
                  risk_factor_class = risk_factor_class)

  # Filter for specific risk factor class
  df_filtered <- df[df$riskClass == risk_factor_class, ]

  plot_time_series(
    df_filtered,
    x_axis = x_axis,
    y_axis = "total.number",
    split_by = split_by,
    age = age,
    year = year
  ) +
    labs(
      title = paste("Prevalence of risk factor class", risk_factor_class),
      y = "Prevalence"
    )
}
