create_transition_matrix_xml <- function(transition_df, type = c("zero", "netto")) {
  matrix_schema_name <- "transitionmatrix"

  matrix_root <- xml2::xml_new_root(matrix_schema_name)

  add_df_to_xml(matrix_root, transition_df, "transition")

  type_schema_name <- paste0("transitionmatrix_", type)

  type_root <- xml2::xml_new_root(type_schema_name)

  validate_xml_schema(matrix_root, matrix_schema_name)
  validate_xml_schema(type_root, type_schema_name)

  return(list(matrix = matrix_root, type = type_root))
}


create_transition_drift_xml <- function(
    transition_df, type = c("zero", "netto"),
    trend = NULL) {
  if (type == "netto" && is.null(trend)) {
    stop("Trend data must be provided for netto transition drifts")
  } else if (type == "zero" && !is.null(trend)) {
    warning("Trend data is not required for zero transition drifts")
  }

  matrix_schema_name <- "transitiondrift"

  matrix_root <- xml2::xml_new_root(matrix_schema_name)

  add_df_to_xml(matrix_root, transition_df, "transition")

  type_schema_name <- paste0("transitiondrift_", type)

  type_root <- xml2::xml_new_root(type_schema_name)

  if (type == "netto" && !is.null(trend)) {
    xml2::xml_add_child(type_root, "trend", trend)
  }

  validate_xml_schema(matrix_root, matrix_schema_name)
  validate_xml_schema(type_root, type_schema_name)

  return(list(matrix = matrix_root, type = type_root))
}


create_relative_risks_death_xml <- function(
    relative_risks_death_df, type = c(
      "continuous", "categorical",
      "compound"
    )) {
  schema_name <- paste0("relrisksfordeath_", type)

  root <- xml2::xml_new_root(schema_name)

  add_df_to_xml(
    root, relative_risks_death_df,
    "relriskfordeath"
  )

  validate_xml_schema(root, schema_name)

  return(root)
}


create_risk_factor_prevalences_xml <- function(
    prevalences_df, type = c("continuous", "categorical", "duration"),
    distribution = c("Normal", "Log normal")) {
  schema_name <- paste0("riskfactorprevalences_", type)

  root <- xml2::xml_new_root(schema_name)

  if (type == "continuous") {
    xml2::xml_add_child(root, "distributiontype", distribution)
    child_node <- xml2::xml_add_child(root, "prevalences")
    add_df_to_xml(child_node, prevalences_df, "prevalence")
  } else {
    add_df_to_xml(root, prevalences_df, "prevalence")
  }
  validate_xml_schema(root, schema_name)

  return(root)
}


create_relative_risks_disability_xml <- function(
    relative_risks_disability_df, type = c(
      "continuous", "categorical",
      "compound"
    )) {
  schema_name <- paste0("relrisksfordisability_", type)

  root <- xml2::xml_new_root(schema_name)

  add_df_to_xml(
    root, relative_risks_disability_df,
    "relriskfordisability"
  )

  validate_xml_schema(root, schema_name)

  return(root)
}


create_risk_factor_configuration <- function(
    configuration_df,
    reference_list,
    type = c("continuous", "categorical", "compound")) {
  if (type == "continuous" && length(reference_list) != 1 &&
    names(reference_list) != "referencevalue") {
    stop("The reference list must contain a single reference value for continuous risk factors")
  } else if (type == "categorical" && length(reference_list) != 1 &&
    names(reference_list) != "referenceclass") {
    stop("The reference list must contain a single reference class for categorical risk factors")
  } else if (type == "compound" && length(reference_list) != 2 &&
    names(reference_list) != c("referenceclass", "referenceduration")) {
    stop(
      "The reference list must contain a reference class and a reference duration for compound risk factors"
    )
  }

  schema_name <- paste0("riskfactor_", type)

  root <- xml2::xml_new_root(schema_name)

  root_name <- switch(type,
    continuous = "cutoffs",
    categorical = "classes",
    compound = "classes"
  )

  child_node <- xml2::xml_add_child(root, root_name)

  child_name <- switch(type,
    continuous = "cutoff",
    categorical = "class",
    compound = "class"
  )

  add_df_to_xml(child_node, configuration_df, child_name)

  for (reference in names(reference_list)) {
    xml2::xml_add_child(root, reference, reference_list[[reference]])
  }

  return(root)
}


#' Write XML files for a risk factor to a directory
#'
#' This function creates a directory structure for a risk factor and populates it with
#' various XML files containing risk factor data.
#'
#' @param risk_factor_name A character string specifying the name of the risk factor.
#' A directory will be created with this name if it does not already exist.
#' @param transition_matrix_list A list of transition matrices, where each element contains
#'   a list with `'data'` and `'type'` components.
#' @param transition_drift_list A list of transition drifts, where each element contains
#'   a list with `'data'`, `'type'`, and `'trend'` components.
#' @param relative_risks_death_list A list of relative risks for death, where each element
#'   contains a list with `'data'` and `'type'` components.
#' @param prevalences_list A list of prevalences, where each element contains a list with
#'   `'data'`, `'type'`, and `'distribution'` components.
#' @param prevalences_duration_list A list of prevalence durations, where each element
#'   contains a list with a `'data'`` component.
#' @param relative_risks_disability_list A list of relative risks for disability, where
#'   each element contains a list with `'data'` and `'type'` components.
#' @param risk_factor_configuration A list of risk factor configurations containing
#'  `'data'`, `'reference'`, and `'type'` components.
#'
#' @return A logical value: TRUE if the directory structure was successfully created.
#'
#' @details
#' The function creates a directory named after the disease and subdirectories for:
#' \itemize{
#'   \item{Transitions}
#'   \item{Relative_Risks_For_Disability}
#'   \item{Relative_Risks_For_Death}
#'   \item{Prevalences}
#'   \item{DurationDistributions}
#'   \item{configuration.xml}
#' }
#'
#' For each category, the function generates XML files based on the data provided in the
#'  corresponding input list. Each XML file is saved in the appropriate subdirectory.
#'
#' The transition matrix and drift type can be `'zero'` or `'netto'`.
#' The risk factor `'type'` can be `'continuous'`, `'categorical'`, or `'compound'`.
#' For `prevalences_list`, only `'continuous'` and `'categorical'` are valid types.
#'
#' @examples
#' \dontrun{
#' # A list element corresponds to a population
#' transition_matrix_list <- list("NL" = list(data = data.frame(...), type = "zero"))
#' transition_drift_list <- list("NL" = list(data = data.frame(...), type = "zero"))
#'
#' prevalences_list <- list("NL" = list(data = data.frame(...), type = "continuous", distribution = "normal"))
#' prevalences_duration_list <- list("NL" = list(data = data.frame(...)))
#'
#' # A list element corresponds to a risk factor
#' relative_risks_death_list <- list("continuous" = list(data = data.frame(...), type = "continuous"))
#' relative_risks_disability_list <- list("continuous" = list(data = data.frame(...), type = "continuous"))
#'
#' # Configuration for which risk factor element to use
#' risk_factor_configuration <- list(data = data.frame(...), reference = list(referencevalue = 0.5), type = "continuous")
#'
#' create_risk_factor_dir(
#'   "smoking",
#'   transition_matrix_list = transition_matrix_list,
#'   transition_drift_list = transition_drift_list,
#'   relative_risks_death_list = relative_risks_death_list,
#'   prevalences_list = prevalences_list,
#'   prevalences_duration_list = prevalences_duration_list,
#'   relative_risks_disability_list = relative_risks_disability_list,
#'   risk_factor_configuration_list = risk_factor_configuration
#' )
#' }
#'
#' @export
write_risk_factor_dir <- function(
    risk_factor_name, transition_matrix_list, transition_drift_list,
    relative_risks_death_list, prevalences_list,
    prevalences_duration_list, relative_risks_disability_list,
    risk_factor_configuration) {
  # Create the risk factor directory if it doesn't exist
  if (!dir.exists(risk_factor_name)) {
    dir.create(risk_factor_name)
  }

  # Transition matrices
  transition_dirname <- file.path(risk_factor_name, "Transitions")
  if (!dir.exists(transition_dirname)) {
    dir.create(transition_dirname)
  }

  for (name in names(transition_matrix_list)) {
    transition <- create_transition_matrix_xml(
      transition_matrix_list[[name]][["data"]],
      transition_matrix_list[[name]][["type"]]
    )
    xml2::write_xml(
      transition[["matrix"]], file.path(
        transition_dirname, paste0(
          "Transition_Matrix_", name, "_",
          basename(risk_factor_name),
          ".xml"
        )
      )
    )
    xml2::write_xml(
      transition[["type"]], file.path(
        transition_dirname, paste0(
          "Transition_", name, "_",
          basename(risk_factor_name),
          "_", tools::toTitleCase(transition_matrix_list[[name]][["type"]]),
          ".xml"
        )
      )
    )
  }

  for (name in names(transition_drift_list)) {
    transition <- create_transition_drift_xml(
      transition_drift_list[[name]][["data"]],
      transition_drift_list[[name]][["type"]],
      transition_drift_list[[name]][["trend"]]
    )
    xml2::write_xml(
      transition[["matrix"]], file.path(
        transition_dirname, paste0(
          "Transition_Matrix_Drift_", name, "_",
          basename(risk_factor_name),
          ".xml"
        )
      )
    )
    xml2::write_xml(
      transition[["type"]], file.path(
        transition_dirname, paste0(
          "Transition_Drift_", name, "_",
          basename(risk_factor_name),
          "_", tools::toTitleCase(transition_drift_list[[name]][["type"]]),
          ".xml"
        )
      )
    )
  }

  # Relative risks for death
  relative_risks_death_dirname <- file.path(risk_factor_name, "Relative_Risks_For_Death")
  if (!dir.exists(relative_risks_death_dirname)) {
    dir.create(relative_risks_death_dirname)
  }

  for (name in names(relative_risks_death_list)) {
    xml2::write_xml(
      create_relative_risks_death_xml(
        relative_risks_death_list[[name]][["data"]],
        relative_risks_death_list[[name]][["type"]]
      ),
      file.path(
        relative_risks_death_dirname, paste0(
          "Relative_Risk_Death_", name,
          "_", basename(risk_factor_name),
          ".xml"
        )
      )
    )
  }

  # Prevalences
  prevalences_dirname <- file.path(risk_factor_name, "Prevalences")
  if (!dir.exists(prevalences_dirname)) {
    dir.create(prevalences_dirname)
  }

  for (name in names(prevalences_list)) {
    xml2::write_xml(
      create_risk_factor_prevalences_xml(
        prevalences_list[[name]][["data"]],
        prevalences_list[[name]][["type"]],
        prevalences_list[[name]][["distribution"]]
      ),
      file.path(
        prevalences_dirname, paste0(
          name, "_", risk_factor_name,
          "_Prevalences.xml"
        )
      )
    )
  }

  # Prevalence durations
  prevalences_duration_dirname <- file.path(risk_factor_name, "DurationDistributions")
  if (!dir.exists(prevalences_duration_dirname)) {
    dir.create(prevalences_duration_dirname)
  }

  for (name in names(prevalences_duration_list)) {
    xml2::write_xml(
      create_risk_factor_prevalences_xml(
        prevalences_duration_list[[name]][["data"]],
        type = "duration"
      ),
      file.path(
        prevalences_duration_dirname, "durationprevalence.xml"
      )
    )
  }

  # Relative risks for disability
  relative_risks_disability_dirname <- file.path(risk_factor_name, "Relative_Risks_For_Disability")
  if (!dir.exists(relative_risks_disability_dirname)) {
    dir.create(relative_risks_disability_dirname)
  }

  for (name in names(relative_risks_disability_list)) {
    xml2::write_xml(
      create_relative_risks_disability_xml(
        relative_risks_disability_list[[name]][["data"]],
        relative_risks_disability_list[[name]][["type"]]
      ),
      file.path(
        relative_risks_disability_dirname,
        paste0(
          "Relative_Risk_Disability_",
          name, "_", basename(risk_factor_name),
          ".xml"
        )
      )
    )
  }

  # Risk factor configurations
  xml2::write_xml(
    create_risk_factor_configuration(
      risk_factor_configuration[["data"]],
      risk_factor_configuration[["reference"]],
      risk_factor_configuration[["type"]]
    ),
    file.path(risk_factor_name, "configuration.xml")
  )

  return(TRUE)
}
