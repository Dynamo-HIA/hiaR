create_newborns_xml <- function(sex_ratio, starting_year, newborns_df) {
  schema_name <- "newborns"

  root <- xml2::xml_new_root(schema_name)

  # Add sexratio and startingYear elements
  xml2::xml_add_child(root, "sexratio", sex_ratio)
  xml2::xml_add_child(root, "startingYear", starting_year)

  # Create the amounts element
  amounts <- xml2::xml_add_child(root, "amounts")

  add_df_to_xml(amounts, newborns_df, "amount")

  validate_xml_schema(root, schema_name)

  return(root)
}


create_population_size_xml <- function(size_df) {
  schema_name <- "populationsize"

  root <- xml2::xml_new_root(schema_name)

  add_df_to_xml(root, size_df, "size")

  validate_xml_schema(root, schema_name)

  return(root)
}


create_overall_mortality_xml <- function(mortality_df) {
  schema_name <- "overallmortality"

  root <- xml2::xml_new_root(schema_name)

  add_df_to_xml(root, mortality_df, "mortality")

  validate_xml_schema(root, schema_name)

  return(root)
}


create_overall_disability_xml <- function(disability_df) {
  schema_name <- "overalldisability"

  root <- xml2::xml_new_root(schema_name)

  add_df_to_xml(root, disability_df, "weight")

  validate_xml_schema(root, schema_name)

  return(root)
}


#' Write XML files for a population to a directory
#'
#' This function creates a directory structure for a specified population and generates XML files
#' containing data for newborns, population size, overall mortality, and disability.
#' The files are saved in the population directory. The function only creates
#' files for provided arguments and leaves other files as they are.
#'
#' @param population_name A character string representing the name of the population.
#' A directory will be created with this name if it does not already exist.
#' @param sex_ratio A numeric value representing the sex ratio of newborns (male to female ratio).
#' @param starting_year An integer representing the starting year of the newborns data.
#' @param newborns_df A data frame containing data for newborns, typically with columns for the
#' year and the number of newborns.
#' @param size_df A data frame containing population size data, typically with columns for the
#' year and population size.
#' @param mortality_df A data frame containing overall mortality data, typically with columns for
#' the year and mortality rate.
#' @param disability_df A data frame containing overall disability data, typically with columns for
#'  the year and disability rate.
#'
#' @return Returns `TRUE` if the directory and XML files were created successfully.
#'
#' @details The function creates a directory named after the population and generates the following
#'  XML files within it:
#'  \itemize{
#'    \item{`newborns.xml`: Contains data for newborns, including the sex ratio and number of newborns
#' per year.}
#'    \item{`size.xml`: Contains data for population size per year.}
#'    \item{`overallmortality.xml`: Contains overall mortality rates per year.}
#'    \item{`overalldisability.xml`: Contains overall disability rates per year.}
#'  }
#'
#' The function uses helper functions to create XML documents for each type of data.
#' These functions are expected to handle the formatting and structuring of the data into valid XML.
#'
#' @seealso
#' \code{\link{create_disease_dir}}
#'
#' @examples
#' \dontrun{
#' newborns <- data.frame(...)
#' size <- data.frame(...)
#' mortality <- data.frame(...)
#' disability <- data.frame(...)
#'
#' create_population_dir("ExamplePopulation", 1.05, 2009, newborns, size, mortality, disability)
#' }
#'
#' @export
write_population_dir <- function(
    population_name, sex_ratio, starting_year, newborns_df, size_df, mortality_df,
    disability_df) {
  if (!dir.exists(population_name)) {
    dir.create(population_name)
  }

  xml2::write_xml(
    create_newborns_xml(sex_ratio, starting_year, newborns_df),
    file.path(population_name, "newborns.xml")
  )
  xml2::write_xml(
    create_population_size_xml(size_df),
    file.path(population_name, "size.xml")
  )
  xml2::write_xml(
    create_overall_mortality_xml(mortality_df),
    file.path(population_name, "overallmortality.xml")
  )
  xml2::write_xml(
    create_overall_disability_xml(disability_df),
    file.path(population_name, "overalldisability.xml")
  )

  return(TRUE)
}
