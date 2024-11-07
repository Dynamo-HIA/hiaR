#' Create a tree of nested files and directories
#'
#' Recurses through `tree_root` and returns a nested, named list
#' corresponding to the nesting structure of files. At the leaf nodes,
#' which can be empty directories or files, it returns NULL.
#'
#' @param tree_root  Path to the root of the tree.
#'
#' @return A nested list. The leaf nodes are either empty directories or (xml) file names.
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
collect_relative_risks <- function(in_list, keyword_map) {
  sapply(names(keyword_map), function(keyword) {
    short_keyword <- keyword_map[[keyword]]
    out <- lapply(in_list, function(x)
      x[[keyword]])
    out <- do.call(rbind, out)
    if (!is.null(short_keyword)) {
      out$to <- short_keyword
    }
    else {
      out$to <- row.names(out)
      out$to <- sub("\\.[0-9]+$", "", out$to)
    }
    rownames(out) <- NULL
    return(out)
  }, simplify = FALSE, USE.NAMES = TRUE)
}
