#' Get all reference data
#'
#' Function that loads all metadata of reference data for a model.
#' The output of the function can be used by the Shiny app to display
#' options to the user.
#' The function organizes files from a specified root directory into predefined groups:
#' diseases, populations, risk factors, and relative risks. It creates a nested list
#' structure representing the file hierarchy within each group.
#'
#' @param root_dir A string specifying the path to the root directory containing the files to be grouped.
#'
#' @return A list with four main components:
#' \itemize{
#'   \item diseases: A nested list representing the file structure under the "Diseases" directory
#'   \item populations: A nested list representing the file structure under the "Populations" directory
#'   \item risk_factors: A nested list representing the file structure under the "Risk_Factors" directory
#'   \item relative_risks: A list with two components:
#'     \itemize{
#'       \item diseases: Relative risks extracted from the diseases group
#'       \item risk_factors: Relative risks extracted from the risk factors group
#'     }
#'   }
#'
#' @details
#' The function uses predefined keywords to identify and extract relative risk information
#' from the diseases and risk factors groups. For diseases, it looks for "Relative_Risks_From_Risk_Factor"
#' and "Relative_Risks_From_Diseases". For risk factors, it searches for "Relative_Risks_For_Death"
#' and "Relative_Risks_For_Disability".
#'
#' @examples
#' \dontrun{
#' root_dir <- "../data/Tutorial_Data/Reference_Data"
#' file_groups <- get_reference_data(root_dir)
#' }
#'
#' @keywords internal
#'
get_reference_data <- function(root_dir) {
  groups <- list(
    diseases = "Diseases",
    populations = "Populations",
    risk_factors = "Risk_Factors"
  )

  groups <- sapply(groups, function(x) {
    get_file_tree(file.path(root_dir, x))
  }, USE.NAMES = TRUE)

  kw_map_diseases <- list(
    Relative_Risks_From_Risk_Factor = NULL,
    Relative_Risks_From_Diseases = NULL
  )
  rr_diseases <- get_relative_risk_source(groups$diseases, names(kw_map_diseases))
  rr_diseases <- collect_relative_risks(rr_diseases, kw_map_diseases)

  kw_map_risk_factors <- list(
    Relative_Risks_For_Death = "death",
    Relative_Risks_For_Disability = "disability"
  )
  rr_risk_factors <- get_relative_risk_source(groups$risk_factors, names(kw_map_risk_factors), FALSE)
  rr_risk_factors <- collect_relative_risks(rr_risk_factors, kw_map_risk_factors)

  groups[["relative_risks"]] <- list(diseases = rr_diseases, risk_factors = rr_risk_factors)

  return(groups)
}
