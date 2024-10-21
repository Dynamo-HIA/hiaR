create_relative_risks_from_risk_factor_xml <- function(relative_risks_risk_factor_df,
                                                       type = c(
                                                         "continuous",
                                                         "continuous4p",
                                                         "categorical",
                                                         "categorical4p",
                                                         "compound",
                                                         "compound4p"
                                                       )) {
  schema_name <- paste0("relrisksfromriskfactor_", type)

  root <- xml2::xml_new_root(schema_name)

  add_df_to_xml(root, relative_risks_risk_factor_df, "relativerisk")

  validate_xml_schema(root, schema_name)

  return(root)
}


create_relative_risks_from_diseases_xml <- function(relative_risks_diseases_df) {
  schema_name <- "relrisksfromdisease"

  root <- xml2::xml_new_root(schema_name)

  add_df_to_xml(root, relative_risks_diseases_df, "relativerisk")

  validate_xml_schema(root, schema_name)

  return(root)
}


create_disease_prevalences_xml <- function(disease_prevalences_df) {
  schema_name <- "diseaseprevalences"

  root <- xml2::xml_new_root(schema_name)

  add_df_to_xml(root, disease_prevalences_df, "prevalence")

  validate_xml_schema(root, schema_name)

  return(root)
}


create_disease_incidences_xml <- function(disease_incidences_df) {
  schema_name <- "diseaseincidences"

  root <- xml2::xml_new_root(schema_name)

  add_df_to_xml(root, disease_incidences_df, "incidence")

  validate_xml_schema(root, schema_name)

  return(root)
}


create_disease_excess_mortalities_xml <- function(unit_type,
                                                parameter_type,
                                                disease_excess_mortalities_df) {
  schema_name <- "excessmortality"

  root <- xml2::xml_new_root(schema_name)

  xml2::xml_add_child(root, "unittype", unit_type)
  xml2::xml_add_child(root, "parametertype", parameter_type)

  child <- xml2::xml_add_child(root, "mortalities")

  add_df_to_xml(child, disease_excess_mortalities_df, "mortality")

  validate_xml_schema(root, schema_name)

  return(root)
}


create_disease_disability_xml <- function(disease_disability_df) {
  schema_name <- "dalyweights"

  root <- xml2::xml_new_root(schema_name)

  add_df_to_xml(root, disease_disability_df, "weight")

  validate_xml_schema(root, schema_name)

  return(root)
}


#' Create directory with XML files for a disease
#'
#' This function creates a directory structure for a specified disease and generates XML files for relative risks from risk factors and other diseases, as well as for disease prevalences, incidences, excess mortalities, and disability data. The directories and XML files are named and organized according to the input lists provided for each category.
#'
#' @param disease_name A character string representing the name of the disease. A directory will be created with this name if it does not already exist.
#' @param risk_factor_list A named list where each element is a list containing two components: `"data"` (the risk factor data) and `"type"` (the type of risk factor). XML files will be generated for each risk factor.
#' @param diseases_list A named list where each element is a list containing disease data under the key `"data"`. XML files will be generated for relative risks from other diseases.
#' @param prevalences_list A named list where each element is a list containing prevalence data under the key `"data"`. XML files will be generated for each prevalence.
#' @param incidences_list A named list where each element is a list containing incidence data under the key `"data"`. XML files will be generated for each incidence.
#' @param excess_mortalities_list A named list where each element is a list containing three components: `"unit_type"` (the unit of the data), `"parameter_type"` (the type of parameter), and `"data"` (the excess mortality data). XML files will be generated for each excess mortality.
#' @param disability_list A named list where each element is a list containing disability data under the key `"data"`. XML files will be generated for each disability dataset.
#'
#' @return Returns `TRUE` if the directory structure and XML files were created successfully.
#'
#' @details The function creates a directory named after the disease and subdirectories for:
#' - Relative_Risks_From_Risk_Factor
#' - Relative_Risks_From_Diseases
#' - Prevalences
#' - Incidences
#' - Excess_Mortalities
#' - Disabilities
#'
#' For each category, the function generates XML files based on the data provided in the corresponding input list. Each XML file is saved in the appropriate subdirectory.
#'
#' The risk factor `"type"` can be `"continuous"`, `"continuous4p"`, `"categorical"`, `"categorical4p"`, `"compound"`, or `"compound4p"`. The excess mortality `"parameter_type"` can be `"Acutely Fatal"` or `"Cured Fraction"`.
#'
#' @seealso
#' \code{\link{create_population_dir}}
#'
#' @examples
#' \dontrun{
#' # A list element corresponds to a risk factor (e.g., smoking)
#' risk_factors <- list("smoking" = list(data = data.frame(...), type = "categorical"))
#'
#' # A list element corresponds to another disease (e.g., diabetes)
#' diseases <- list("diabetes" = list(data = data.frame(...)))
#'
#' # A list element corresponds to a population (e.g., Netherlands)
#' prevalences <- list("NL" = list(data = data.frame(...)))
#' incidences <- list("NL" = list(data = data.frame(...)))
#' mortalities <- list("NL" = list(unit_type = "Rate", parameter_type = "Acutely Fatal", data = data.frame(...)))
#' disabilities <- list("NL" = list(data = data.frame(...)))
#'
#' create_disease_dir("stroke", risk_factors, diseases, prevalences, incidences, mortalities, disabilities)
#' }
#'
#' @export

create_disease_dir <- function(disease_name,
                               risk_factor_list,
                               diseases_list,
                               prevalences_list,
                               incidences_list,
                               excess_mortalities_list,
                               disability_list) {
  if (!dir.exists(disease_name)) {
    dir.create(disease_name)
  }

  # Risk factors
  relative_risks_dirname <- file.path(disease_name, "Relative_Risks_From_Risk_Factor")

  if (!dir.exists(relative_risks_dirname)) {
    dir.create(relative_risks_dirname)
  }

  for (name in names(risk_factor_list)) {
    xml2::write_xml(
      create_relative_risks_from_risk_factor_xml(
        risk_factor_list[[name]][["data"]],
        risk_factor_list[[name]][["type"]]
      ),
      file.path(
        relative_risks_dirname,
        paste0(
          "Relative_Risk_", name, "_", basename(disease_name), "-", name, ".xml")
      )
    )
  }

  # Other diseases
  other_diseases_dirname <- file.path(disease_name, "Relative_Risks_From_Diseases")

  if (!dir.exists(other_diseases_dirname)) {
    dir.create(other_diseases_dirname)
  }

  for (name in names(diseases_list)) {
    xml2::write_xml(
      create_relative_risks_from_diseases_xml(
        diseases_list[[name]][["data"]]
      ),
      file.path(
        other_diseases_dirname,
        paste0(
          "Relative_Risk_", name, "_", basename(disease_name), "-", name, ".xml")
      )
    )
  }

  # Prevalences
  prevalences_dirname <- file.path(disease_name, "Prevalences")

  if (!dir.exists(prevalences_dirname)) {
    dir.create(prevalences_dirname)
  }

  for (name in names(prevalences_list)) {
    xml2::write_xml(
      create_disease_prevalences_xml(
        prevalences_list[[name]][["data"]]
      ),
      file.path(
        prevalences_dirname,
        paste(
          name, disease_name, "Prevalences.xml", sep = "_"
        )
      )
    )
  }

  # Prevalences
  incidences_dirname <- file.path(disease_name, "Incidences")

  if (!dir.exists(incidences_dirname)) {
    dir.create(incidences_dirname)
  }

  for (name in names(incidences_list)) {
    xml2::write_xml(
      create_disease_incidences_xml(
        incidences_list[[name]][["data"]]
      ),
      file.path(
        incidences_dirname,
        paste(
          name, disease_name, "Incidences.xml", sep = "_"
        )
      )
    )
  }

  # Excess mortality
  mortalities_dirname <- file.path(disease_name, "Excess_Mortalities")

  if (!dir.exists(mortalities_dirname)) {
    dir.create(mortalities_dirname)
  }

  for (name in names(excess_mortalities_list)) {
    xml2::write_xml(
      create_disease_excess_mortalities_xml(
        excess_mortalities_list[[name]][["unit_type"]],
        excess_mortalities_list[[name]][["parameter_type"]],
         excess_mortalities_list[[name]][["data"]]
      ),
      file.path(
        mortalities_dirname,
        paste(
          name, disease_name, "Mortalities.xml", sep = "_"
        )
      )
    )
  }

  # Disability
  disability_dirname <- file.path(disease_name, "Disability")

  if (!dir.exists(disability_dirname)) {
    dir.create(disability_dirname)
  }

  for (name in names(disability_list)) {
    xml2::write_xml(
      create_disease_disability_xml(
        disability_list[[name]][["data"]]
      ),
      file.path(
        disability_dirname,
        paste(
          name, disease_name, "Disability.xml", sep = "_"
        )
      )
    )
  }

  return(TRUE)
}
