#' Create a tree of nested files and directories
#'
#' Recurses through `tree_root` and returns a nested, named list
#' corresponding to the nesting structure of files. At the leaf nodes,
#' which can be empty directories or files, it returns NULL.
#'
#' @param tree_root  Path to the root of the tree.
#'
#' @return A nested list. The leaf nodes are either empty directories or (xml) file names.
#'
#' @keywords internal
#'
get_file_tree <- function(tree_root) {
  contents <- list.files(
    tree_root,
    recursive = FALSE,
    full.names = FALSE,
    include.dirs = TRUE
  )

  result <- lapply(contents, function(x) {
    target <- file.path(tree_root, x)
    if (dir.exists(target)) {
      sub_contents <- get_file_tree(target)
      if (length(sub_contents) > 0) {
        return(sub_contents)
      }
    }
    return(NULL)
  })

  names(result) <- contents

  return(result)
}



#' Extract the source of relative risk transitions
#'
#' Extracts the substring after the unique hyphen in the file name.
#'
#' @param file_name The file name as a string.
#'
#' @return A string. An error is thrown if the file name is not in the expected format or does
#' not have the right ending.
#'
#' @examples
#' \dontrun{
#' extract_relative_risk_source("RR_to_BreatCa-Smoking_dur.xml")
#' }
#'
#' @keywords internal
#'
extract_relative_risk_source <-
  function(file_name) {
    validate_file_ending(file_name, ".xml")
    validate_n_substrings(file_name, "-", 1)

    file_name <- gsub(".xml", "", file_name)
    splitted <- strsplit(file_name, "-")
    return(splitted[[1]][2])
  }

#' Get the "source" for relative risk flows
#'
#' Function to extract relative risk information from a nested list structure.
#' It returns the filename and the source of the relative risk. The source
#' is extracted from the file name when `extract` is TRUE, and
#' otherwise from the item containing the relative risk information.
#'
#' @param input_list A nested list containing relative risk information.
#' @param lookup_items A character vector of items to look up within each sublist.
#' @param extract Logical. If TRUE (default), extracts the source of relative risks, by calling \link{extract_relative_risk_source}.
#'                If FALSE, returns the name of the item containing the relative risk.
#'
#' @return A nested list structure containing the extracted information or item names at the leaf nodes.
#' If an empty directory is the leaf node, its value is an empty list.
#'
#' @examples
#' \dontrun{
#' groups <- get_file_groups("../data/Tutorial_Data/Reference_Data")
#' keywords_disease <- c("Relative_Risks_From_Risk_Factor", "Relative_Risks_From_Diseases")
#' keywords_rf <- c("Relative_Risks_For_Death", "Relative_Risks_For_Disability")
#' get_relative_risk_source(groups$diseases, keywords_disease)
#' get_relative_risk_source(groups$risk_factors, keywords_rf, FALSE)
#' }
#'
#' @keywords internal
#'
get_relative_risk_source <- function(input_list, lookup_items, extract = TRUE) {
  stopifnot(
    is.list(input_list),
    is.character(lookup_items),
    is.logical(extract)
  )

  sapply(names(input_list), function(item_name) {
    sublist <- input_list[[item_name]]
    sapply(lookup_items, function(item) {
      indicators <- names(sublist[[item]])
      if (length(indicators) > 0) {
        if (isTRUE(extract)) {
          from <- sapply(indicators, extract_relative_risk_source, USE.NAMES = FALSE)
        }
        else {
          from <- rep(item_name, length(indicators))
        }
        data.frame(
          from = from,
          filename = indicators,
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    }, USE.NAMES = TRUE, simplify = FALSE)
  }, USE.NAMES = TRUE, simplify = FALSE)
}

#' Collect files on relative risks in compact lists
#'
#' The function iterates over the keys in `keyword_map`. For each key, it iterates
#' over the elements in `in_list`, extracts the element named by the key and
#' binds the dataframes into one, using rbind.
#' It requires that all keys in `keyword_map` are present as list names the lists of `in_list`.
#'
#'
#' @param in_list A list of lists, where the upper most level are names of
#' risk factors or diseases, and each list contains one or multiple lists corresponding
#' to the names in `mapping`.
#' @param keyword_map A named list that maps list names on the second nesting of `in_list` (the keys) to
#' shorter names, or NULL (the values). If the values are not NULL, it is assigned to the `to`
#' value of the entire list in the output. If values are NULL, the names of the list
#' that are being iterated over are assigned to `to` in the output.
#'
#' @return A list with 2 elements, with the names coming from the names of `keyword_map`.
#' The two lists contain a list of 3, named `from`, `to`, and `filename`. They indicate
#' the name of the file that contains the relative risks for going from risk factors
#' to diseases, between diseases, or from diseases to death/disability. If for any
#' type of relative risk there is no file present, the list is empty.
#'
#' @keywords internal
#'
collect_relative_risks <- function(in_list, keyword_map) {
  sapply(names(keyword_map), function(keyword) {
    short_keyword <- keyword_map[[keyword]]
    out <- lapply(in_list, function(x)
      x[[keyword]])
    out <- do.call(rbind, out)
    if (is.null(out)) {
      return(out)
    } else{
      if (!is.null(short_keyword)) {
        out$to <- short_keyword
      }
      else {
        out$to <- row.names(out)
        out$to <- sub("\\.[0-9]+$", "", out$to)
      }
      rownames(out) <- NULL
      return(out)
    }
  }, simplify = FALSE, USE.NAMES = TRUE)
}

get_directory_names <- function(dir_name, return_paths = FALSE) {
  tree <- fs::dir_tree(dir_name, type = "directory", recurse = FALSE)
  if (return_paths) {
    return(invisible(tree))
  }
  return(invisible(fs::path_file(tree)))
}

#' Get disease directory names
#'
#' Retrieves the names or paths of disease directories within the project structure.
#'
#' @param root_dir A character string which specifies the root directory of the project.
#' @param return_paths A logical which determines whether to return full paths (TRUE) or
#' only directory names (FALSE).
#'
#' @return An invisible character vector of disease directory names or paths.
#'
#' @export
#'
#' @examples
#' root_dir <- system.file("extdata", "example-nl", "Reference_Data", package = "hiaR")
#' get_disease_names(root_dir)
#' get_disease_names(root_dir, return_paths = TRUE)
#'
get_disease_names <- function(root_dir, return_paths = FALSE) {
  return(get_directory_names(fs::path(root_dir, "Diseases"), return_paths))
}

#' Get population directory names
#'
#' Retrieves the names or paths of population directories within the project structure.
#'
#' @param root_dir A character string which specifies the root directory of the project.
#' @param return_paths A logical which determines whether to return full paths (TRUE) or
#' only directory names (FALSE).
#'
#' @return An invisible character vector of population directory names or paths.
#'
#' @export
#'
#' @examples
#' root_dir <- system.file("extdata", "example-nl", "Reference_Data", package = "hiaR")
#' get_population_names(root_dir)
#' get_population_names(root_dir, return_paths = TRUE)
#'
get_population_names <- function(root_dir, return_paths = FALSE) {
  return(get_directory_names(fs::path(root_dir, "Populations"), return_paths))
}

#' Get risk factor directory names
#'
#' Retrieves the names or paths of risk factor directories within the project structure.
#'
#' @param root_dir A character string which specifies the root directory of the project.
#' @param return_paths A logical which determines whether to return full paths (TRUE) or
#' only directory names (FALSE).
#'
#' @return An invisible character vector of risk factor directory names or paths.
#'
#' @export
#'
#' @examples
#' root_dir <- system.file("extdata", "example-nl", "Reference_Data", package = "hiaR")
#' get_risk_factor_names(root_dir)
#' get_risk_factor_names(root_dir, return_paths = TRUE)
#'
get_risk_factor_names <- function(root_dir, return_paths = FALSE) {
  return(get_directory_names(fs::path(root_dir, "Risk_Factors"), return_paths))
}

#' Get simulation directory names
#'
#' Retrieves the names or paths of simulation directories within the project structure.
#'
#' @param root_dir A character string which specifies the root directory of the project.
#' @param return_paths A logical which determines whether to return full paths (TRUE) or
#' only directory names (FALSE).
#'
#' @return An invisible character vector of simulation directory names or paths.
#'
#' @export
#'
get_simulation_names <- function(root_dir, return_paths = FALSE) {
  return(get_directory_names(fs::path(root_dir, "Simulations"), return_paths))
}

get_directory_filenames <- function(root_dir, dir_name, sub_dir_name) {
  return(fs::dir_tree(fs::path(root_dir, dir_name, sub_dir_name)))
}

#' Get disease filenames
#'
#' Retrieves all filenames associated with a specific disease.
#'
#' @param root_dir A character string which specifies the root directory of the project.
#' @param disease_name A character string which specifies the name of the disease directory.
#'
#' @return A character vector of filenames associated with the specified disease.
#'
#' @export
#'
#' @examples
#' root_dir <- system.file("extdata", "example-nl", "Reference_Data", package = "hiaR")
#' get_disease_filenames(root_dir, "Lung_Cancer")
#'
get_disease_filenames <- function(root_dir, disease_name) {
  return(get_directory_filenames(root_dir, "Diseases", disease_name))
}

#' Get population filenames
#'
#' Retrieves all filenames associated with a specific population.
#'
#' @param root_dir A character string which specifies the root directory of the project.
#' @param population_name A character string which specifies the name of the population directory.
#'
#' @return A character vector of filenames associated with the specified population.
#'
#' @export
#'
#' @examples
#' root_dir <- system.file("extdata", "example-nl", "Reference_Data", package = "hiaR")
#' get_population_filenames(root_dir, "Netherlands")
#'
get_population_filenames <- function(root_dir, population_name) {
  return(get_directory_filenames(root_dir, "Populations", population_name))
}

#' Get risk factor filenames
#'
#' Retrieves all filenames associated with a specific risk factor.
#'
#' @param root_dir A character string which specifies the root directory of the project.
#' @param risk_factor_name A character string which specifies the name of the risk factor directory.
#'
#' @return A character vector of filenames associated with the specified risk factor.
#'
#' @export
#'
#' @examples
#' root_dir <- system.file("extdata", "example-nl", "Reference_Data", package = "hiaR")
#' get_risk_factor_filenames(root_dir, "Smoking_cat3")
#'
get_risk_factor_filenames <- function(root_dir, risk_factor_name) {
  return(get_directory_filenames(root_dir, "Risk_Factors", risk_factor_name))
}

#' Configure a risk factor
#'
#' @description Creates a configuration list for a risk factor in the simulation.
#'
#' @param name A character string which specifies the unique name of the risk factor.
#' @param transition_filename A character string which specifies the path to the
#' transition rates file.
#' @param prevalence_filename A character string which specifies the path to the prevalence file.
#'
#' @return A list containing the risk factor configuration parameters.
#'
#' @export
#'
#' @examples
#' configure_risk_factor(
#'   name = "Smoking_cat3", # Should be the same as the name in the reference data
#'   transition_filename = "NL_RF_smoking_Transitions_Netto",
#'   prevalence_filename = "NL_RF_smoking_cat3_Prev_V1"
#' )
#'
configure_risk_factor <- function(name, transition_filename, prevalence_filename) {
  return(list(
    uniquename = name,
    transfilename = transition_filename,
    prevfilename = prevalence_filename
  ))
}

#' Configure a disease
#'
#' @description Creates a configuration list for a disease in the simulation.
#'
#' @param name A character string which specifies the unique name of the disease.
#' @param prevalence_filename A character string which specifies the path to the prevalence file.
#' @param incidence_filename A character string which specifies the path to the incidence file.
#' @param excess_mortality_filename A character string which specifies the path to the
#' excess mortality file.
#' @param disability_weights_filename A character string which specifies the path to the
#' disability weights file.
#'
#' @return A list containing the disease configuration parameters.
#'
#' @export
#'
#' @examples
#' configure_disease(
#'   name = "Lung_Cancer",
#'   prevalence_filename = "NL_disease_LungCa_Prev_V2",
#'   incidence_filename = "NL_disease_LungCa_Inc_V2",
#'   excess_mortality_filename = "NL_disease_LungCa_ExecMor_V2",
#'   disability_weights_filename = "NL_disease_LungCa_DALY_V1"
#' )
#'
configure_disease <- function(name,
                              prevalence_filename,
                              incidence_filename,
                              excess_mortality_filename,
                              disability_weights_filename) {
  return(list(
    uniquename = name,
    prevfilename = prevalence_filename,
    incfilename = incidence_filename,
    excessmortfilename = excess_mortality_filename,
    dalyweightsfilename = disability_weights_filename
  ))
}

#' Configure a relative risk
#'
#' @description Creates a configuration list for a relative risk relationship between a
#' risk factor and disease.
#'
#' @param index A character string which specifies the unique identifier for this
#' relative risk relationship.
#' @param from A character string which specifies the source (usually a risk factor).
#' @param to A character string which specifies the target (usually a disease).
#' @param relative_risk_filename A character string which specifies the path to the
#' relative risk file.
#'
#' @return A list containing the relative risk configuration parameters.
#'
#' @export
#'
#' @examples
#' configure_relative_risk(
#'   index = 0,
#'   from = "Smoking_cat3",
#'   to = "Lung_Cancer",
#'   relative_risk_filename = "RR_to_LungCa-Smoking_cat3"
#' )
#'
configure_relative_risk <- function(index, from, to, relative_risk_filename) {
  return(list(
    RRindex = index,
    isRRfrom = from,
    isRRto = to,
    isRRFile = relative_risk_filename
  ))
}

#' Configure a scenario
#'
#' @description Creates a configuration list for an intervention scenario in the simulation.
#'
#' @param name A character string which specifies the unique name of the scenario.
#' @param transition_filename A character string which specifies the path to the
#' transition rates file.
#' @param prevalence_filename A character string which specifies the path to the prevalence file.
#' @param success_rate A numeric which specifies the intervention success rate (0-100).
#' Default is 100.
#' @param min_age A numeric which specifies the minimum target age (0-95). Default is 0.
#' @param max_age A numeric which specifies the maximum target age (0-95). Default is 95.
#' @param gender A numeric which specifies the target gender (0=female, 1=male, 2=both).
#' Default is 2 (both).
#'
#' @return A list containing the scenario configuration parameters.
#'
#' @export
#'
#' @examples
#' configure_scenario(
#'   name = "All_Never_Smokers",
#'   success_rate = 80,
#'   min_age = 18,
#'   max_age = 65,
#'   gender = 2, # Both female and male
#'   transition_filename = "NL_RF_smoking_Transitions_Netto",
#'   prevalence_filename = "RF_smoking_cat_All_Never_Smokers_Prev"
#' )
#'
configure_scenario <- function(name,
                               transition_filename,
                               prevalence_filename,
                               success_rate = 100,
                               min_age = 0,
                               max_age = 95,
                               gender = 2) {
  return(list(
    uniquename = name,
    successRate = success_rate,
    targetMinAge = min_age,
    targetMaxAge = max_age,
    targetSex = gender,
    transfilename = transition_filename,
    prevfilename = prevalence_filename
  ))
}
