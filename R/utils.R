#' Add a data frame to an XML document
#'
#' This function appends rows from a data frame as child elements to a specified root XML node. Each row is added as a new child element with column values either as attributes or child elements, depending on the structure of the data.
#'
#' @param root An XML node (created with \code{\link[xml2]{xml_new_root}} or similar) that will serve as the root element to which the data frame rows will be added.
#' @param df A data frame where each row represents a set of values to be added as a child element to the root XML node. The column names of the data frame are used as the names of the child elements.
#' @param row_name A character string representing the name to be used for each row of the data frame in the XML structure. Each row will be added as a child element under the root node with this name.
#'
#' @return The modified root XML node with the added rows from the data frame.
#'
#' @details
#' For each row in the data frame, a new child node is created under the `root` element. Each column in the data frame becomes a child element under that row's node, with the column name used as the tag and the value in the data frame used as the content of that tag. If a value is `NA`, that column is skipped for that row.
#'
#' The function is designed to handle simple data frames where each cell contains atomic values (strings, numbers, etc.). It does not handle complex or nested structures.
#'
#' @examples
#' \dontrun{
#' # Example for disease prevalence across age and sex groups
#' root <- xml2::xml_new_root("prevalences")
#' df <- data.frame(age = c(21, 21, 22, 22), sex = c(0, 1, 0, 1), value = c(1000, 2000, 1500, 3000))
#' root <- add_df_to_xml(root, df, "prevalence")
#' }
#'
#' @keywords internal
#'
add_df_to_xml <- function(root, df, row_name) {
  # Iterate over each row in the data frame
  for (i in seq_len(nrow(df))) {
    # Add a new child element for each row
    row_node <- xml2::xml_add_child(root, row_name)

    # Add attributes or child elements for each column
    for (col in names(df)) {
      if (!all(is.na(df[[col]][i]))) {
        xml2::xml_add_child(row_node, col, as.character(df[[col]][i][!is.na(df[[col]][i])]))
      }
    }
  }

  return(root)
}


#' Read an XML Schema from the Package
#'
#' This function reads an XML schema (XSD file) from the `extdata/schemas` directory. It constructs the full file path to the schema and returns the parsed XML document.
#'
#' @param filename A character string representing the base name of the XML schema file (with or without the `.xsd` extension).
#'
#' @return An XML document object representing the parsed XML schema.
#'
#' @details
#' The function locates the specified XML schema file within the `extdata/schemas` directory, optionally appends the `.xsd` extension to the filename, and reads the schema file using `xml2::read_xml`. This is useful for working with pre-defined schemas within the package's directory structure.
#'
#' @examples
#' \dontrun{
#' # Read an XML schema file named 'example_schema' from the package
#' schema <- read_xml_schema("populationsize")
#' }
#'
#' @keywords internal
#'
read_xml_schema <- function(filename) {
  full_filepath <- fs::path_package(
    "extdata/schemas", fs::path_ext_set(filename, "xsd"),
    package = "hiaR"
  )
  return(xml2::read_xml(full_filepath))
}


create_xml_schema_error <- function(schema_name, errors) {
  return(
    paste0(
      "Validation of XML schema ", schema_name, " failed:\n", paste(
        attr(errors, "errors"),
        collapse = "\n"
      )
    )
  )
}


#' Validate an XML document against a schema
#'
#' This function validates an XML document against a specified XML schema. If the XML document does not conform to the schema, the function throws an error with a message generated from the schema name.
#'
#' @param root An XML document object representing the root of the XML to be validated.
#' @param schema_name A character string representing the base name of the XML schema (with or without the `.xsd` extension) against which the XML document will be validated.
#'
#' @return This function returns nothing on success but throws an error if the XML document fails validation.
#'
#' @details
#' The function reads the specified XML schema using `read_xml_schema` and validates the `root` XML document against it using \code{\link[xml2]{xml_validate}}.
#'
#' @examples
#' \dontrun{
#' # Validate an XML document against the 'example_schema' schema
#' root <- xml2::read_xml("<root><child>Example</child></root>")
#' validate_xml_schema(root, "example_schema")
#' }
#'
#' @keywords internal
#'
validate_xml_schema <- function(root, schema_name) {
  is_valid <- xml2::xml_validate(root, read_xml_schema(schema_name))
  if (!is_valid) {
    stop(create_xml_schema_error(schema_name, is_valid))
  }
}


#' Validate that a string has N substrings of a given pattern
#'
#' @param s The string to check.
#' @param pattern The pattern to search for.
#' @param count The number of occurrences of `pattern` in `s`.
#'
#' @return Throws an error if the check fails.
#'
#' @examples
#' \dontrun{
#' validate_n_substrings("hello-world", "-", 1) # passes
#' validate_n_substrings("hello-world", "!", 1) # fails
#' }
#'
#' @keywords internal
#'
validate_n_substrings <- function(s, pattern, count) {
  if (length(gregexpr(pattern, s)) != count) {
    msg <- paste0("Invalid filename:", s, ". Expected to have ", count, "occurences of ", pattern, ".")
    stop(msg)
  }
}


#' Validate the file ending of a file
#'
#' @param file_name String of the file name.
#' @param ending The file name suffix, ie `.xml`.
#'
#' @return Throws an error if validation fails.
#'
#' @examples
#' \dontrun{
#' validate_file_ending("hello.txt", ".xml") # error
#' validate_file_ending("hello.py", ".py") # success
#' }
#'
#' @keywords internal
#'
validate_file_ending <- function(file_name, ending) {
  if (!is.character(file_name)) {
    msg <- paste("Invalid type. Exepcted string but got ", typeof(file_name))
    stop(msg)
  }
  n_chars <- nchar(file_name)
  start <- n_chars - nchar(ending) + 1
  file_ending <- substr(file_name, start, n_chars)


  if (file_ending != ending) {
    msg <- paste("Invalid ending for file:", file_name, ". Expected ", ending, " ending.")
    stop(msg)
  }
}


#' Get return data from a list of servers
#'
#' @param server_name_prefix The prefix for the server names. The function looks
#' for server names with `server_name_prefix` + `i`, where `i` ranges from 1 to
#' the number of servers in `server_list`.
#' @param server_list A list of shiny `moduleServer`s.
#' @param item_names The names of items which each server refers to. For instance,
#' these can be the names of diseases or risk factors.
#'
#' @returns A named list of non-NULL server values.
#'
#' @keywords internal
#'
fetch_server_data <- function(server_name_prefix, server_list, item_names) {
  stopifnot(length(server_list) == length(item_names))

  out_names <- c()
  outputs <- list()

  for (i in seq_along(names(server_list))) {
    server_name <- paste0(server_name_prefix, i)
    values <- tryCatch(server_list[[server_name]](), error = function(e) NULL)
    if (!is.null(values)) {
      current_length <- length(outputs)
      outputs[[current_length + 1]] <- values
      out_names[current_length + 1] <- item_names[i]
    }
  }

  names(outputs) <- out_names
  return(outputs)
}

#' Filter relative risks based on selected diseases and risk factors
#'
#' @param relative_risks A list containing relative risk data.
#' @param diseases A vector of selected disease names.
#' @param risk_factors A vector of selected risk factor names.
#'
#' @returns A list containing data frames:
#'   \item{from_diseases}{Relative risks between diseases}
#'   \item{from_risk_factors}{Relative risks from risk factors to diseases}
#'   \item{to_death}{Relative risks from risk factors to death}
#'   \item{to_disability}{Relative risks from risk factors to disability}
#'
#' @details
#' This function filters the relative risk data based on the provided diseases
#' and risk factors. It returns a subset of the original data that only includes
#' the specified diseases and risk factors. If there are no relative risk for a
#' given category, it returns an empty data frame in that category.
#'
#' @export
#' @keywords internal
filter_relative_risks <- function(relative_risks, diseases, risk_factors) {
  stopifnot(is.list(relative_risks))

  if (
    length(relative_risks) > 0 &&
    length(diseases) > 0 &&
    length(risk_factors) > 0) {
    # into diseases
    rr_from_diseases <- relative_risks$diseases$Relative_Risks_From_Diseases
    if (!is.null(rr_from_diseases)) {
      rr_from_diseases <- subset(rr_from_diseases, from %in% diseases | to %in% diseases)
    } else {
      rr_from_diseases <- data.frame()
    }

    rr_from_risk_factors <- relative_risks$diseases$Relative_Risks_From_Risk_Factor
    if (!is.null(rr_from_risk_factors)) {
      rr_from_risk_factors <- subset(rr_from_risk_factors, from %in% risk_factors & to %in% diseases)
    } else {
      rr_from_risk_factors <- data.frame()
    }

    # from risk factors
    rr_to_death <- relative_risks$risk_factors$Relative_Risks_For_Death
    if (!is.null(rr_to_death)) {
      rr_to_death <- subset(rr_to_death, from %in% risk_factors)
    } else {
      rr_to_death <- data.frame()
    }

    rr_to_disability <- relative_risks$risk_factors$Relative_Risks_For_Disability
    if (!is.null(rr_to_disability)) {
      rr_to_disability <- subset(rr_to_disability, from %in% risk_factors)
    } else {
      rr_to_disability <- data.frame()
    }


    return(list(
      from_diseases = rr_from_diseases,
      from_risk_factors = rr_from_risk_factors,
      to_death = rr_to_death,
      to_disability = rr_to_disability
    ))
  } else {
    return(list())
  }
}

#' Create choices from filtered relative risks
#'
#' @param relative_risks A list containing available relative risk data.
#' @param caller A string indicating the calling context, either "ui" or "server".
#'
#' @returns A list containing four elements:
#'   \item{disease_choices}{A list of choices for relative risks from disease to disease}
#'   \item{risk_factor_choices}{A list of choices for relative risks from risk factor to disease}
#'   \item{death_choices}{A list of choices for relataive risks from risk factor to death}
#'   \item{disability_choices}{A list of choices for relative risks from risk factor to disability}
#'
#' @details
#' This function processes the available relative risk data and creates choice
#' lists. The lists can be used in UI elements such as checkBoxGroupInput.
#' If using `caller = "server"`, it can be used in the server listening to the
#' respective UI to return the file names of the elements chosen by the user.
#'
#' @keywords internal
create_choices_from_relative_risks <- function(
    relative_risks,
    caller = "ui") {
  output <- list(
    from_diseases = list(),
    from_risk_factors = list(),
    to_death = list(),
    to_disability = list()
  )

  if (length(relative_risks) > 0) {
    items <- names(output)
    stopifnot(all(items %in% names(relative_risks)))
    output <- sapply(items, function(x) {
      create_choice_list(relative_risks[[x]], caller = caller)
    }, USE.NAMES = TRUE, simplify = FALSE)
  }

  return(output)
}


#' Create a choice list from a data frame of relative risks
#'
#' @param input_df A data frame containing relative risk data.
#' @param caller A string indicating the calling context, either "ui" or "server".
#'
#' @returns A named list where names are either full descriptions of the relative
#' risks (if `caller = "ui"`) or file names (if `caller = "server"`).
#'
#' @details
#' This function generates a choice list from a data frame of relative risks.
#' The format of the returned list depends on whether it's being called
#' from the UI or server context.
#'
#' @keywords internal
create_choice_list <- function(input_df, caller = "ui") {
  stopifnot(is.data.frame(input_df))

  if (nrow(input_df) == 0) {
    return(list())
  } else {
    choice_list <- sapply(seq_along(input_df$from), function(i) {
      if (caller == "ui") {
        val <- paste0(input_df$from[i], " -> ", input_df$to[i])
      } else if (caller == "server") {
        val <- input_df$filename[i]
      }
      setNames(i, val)
    })
    return(as.list(choice_list))
  }
}


#' Filter a dataframe based on return values from a list of servers.
#'
#' @param df_to_filter A data frame to be filtered by the output of `server_list`.
#' @param server_list A list of servers, each returning a character vector, for instance
#' of user-chosen elements.
#' @param filter_col The column in `df_to_filter` to use for subsetting relative to
#' user input data.
#'
#' @returns A `data.frame`
#'
#' @keywords internal
#'
filter_df_from_server_data <- function(
    df_to_filter, server_list, filter_col = "filename") {
  server_output <- lapply(server_list, function(x) x())
  selected_items <- unlist(server_output, use.names = FALSE)

  if (length(selected_items) > 0 && nrow(df_to_filter) > 0) {
    output_df <- subset(df_to_filter, df_to_filter[[filter_col]] %in% selected_items)
    return(output_df)
  } else {
    return(data.frame())
  }
}


get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "macos"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "macos"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


set_env_path <- function(os) {
  exec_path <- fs::path(switch(os,
                               macos = "DYNAMO-HIA.app",
                               "DYNAMO-HIA"), switch(os,
                                                     windows = "DYNAMO-HIA.exe",
                                                     macos = fs::path("Contents", "MacOS",
                                                                      "DYNAMO-HIA"),
                                                     linux = fs::path("bin", "DYNAMO-HIA"))
  )

  Sys.setenv(DYNAMO_HIA_PATH = exec_path)
}


#' Download and extract GitHub release asset
#'
#' @param repo_url Character string of the repository URL.
#' Default is `"https://github.com/Dynamo-HIA/dynamo-hia"`.
#' @param release_tag Character string of the release tag. Default is `"v3.0.0-beta.1"`.
#' @param os Optional character string of the operating system. Default is `NULL` which tries
#' to detect the OS automatically.
#' @param dest_dir Character string of the destination directory for extraction
#' (default: `tempdir()`).
#' @param make_executable Logical indicating whether to set execute permissions. Default is `TRUE`.
#' @param github_pat Optional GitHub Access Token. Default is `NULL` which will try to use the
#' token from the environment variable `GITHUB_PAT`.
#'
#' @return An invisible `fs::path` object pointing to the extracted contents.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # With default arguments
#' download_github_release()
#'
#' # From specific repository and release tag
#' download_github_release(
#'   repo_url = "https://github.com/Dynamo-HIA/dynamo-hia",
#'   release_tag = "v3.0.0-beta.1"
#' )
#' }
#'
download_github_release <- function(repo_url = "https://github.com/Dynamo-HIA/dynamo-hia",
                                    release_tag = "v3.0.0-beta.1",
                                    os = NULL,
                                    dest_dir = tempdir(),
                                    make_executable = TRUE,
                                    github_pat = NULL) {
  if (is.null(os)) {
    os <- get_os()
  }

  if (is.null(github_pat)) {
    github_pat <- Sys.getenv("GITHUB_PAT")
  }

  asset_pattern <- switch(os,
                          "windows" = "windows-latest-package.zip",
                          "macos" = "macos-latest-package.zip",
                          "linux" = "ubuntu-latest-package.zip",
                          stop("Argument 'os' must be one of 'windows', 'macos', or 'linux'"))

  set_env_path(os)

  # Convert dest_dir to fs::path
  dest_dir <- fs::path(dest_dir)

  # Extract owner and repo from URL
  repo_parts <- strsplit(gsub("https://github.com/", "", repo_url), "/")[[1]]
  owner <- repo_parts[1]
  repo <- repo_parts[2]

  # Construct API URL
  api_url <- sprintf("https://api.github.com/repos/%s/%s/releases/tags/%s",
                     owner, repo, release_tag)

  # Get release information
  release_info <- httr::GET(api_url,
                            httr::add_headers("Accept" = "application/vnd.github.v3+json"))

  if (httr::status_code(release_info) != 200) {
    stop(paste("Failed to fetch release information:\n", release_info))
  }

  # Parse response
  release_data <- httr::content(release_info)

  # Find matching asset
  matching_assets <- Filter(function(x) grepl(asset_pattern, x$name), release_data$assets)

  if (length(matching_assets) == 0) {
    stop("No matching asset found")
  }

  asset <- matching_assets[[1]]

  # Create temporary directory that will be cleaned up automatically
  withr::with_tempfile("download", {
    # Create temp file path with correct extension
    temp_path <- fs::path(download, ext = fs::path_ext(asset$name))

    if (github_pat != "") {
      # Download asset
      download_response <- httr::GET(
        asset$url,
        httr::add_headers(
          "Accept" = "application/octet-stream",
          "Authorization" = sprintf("token %s", github_pat)
        ),
        httr::write_disk(temp_path)
      )
    } else {
      download_response <- httr::GET(
        asset$url,
        httr::add_headers(
          "Accept" = "application/octet-stream"
        ),
        httr::write_disk(temp_path)
      )
    }

    if (httr::status_code(download_response) != 200) {
      stop("Failed to download asset; have you set the 'GITHUB_PAT' environment variable?")
    }

    # Create destination directory if it doesn't exist
    fs::dir_create(dest_dir)

    # Extract if it's a zip file
    if (fs::path_ext(asset$name) == "zip") {
      unzip(temp_path, exdir = dest_dir)

      # Set execute permissions on all files if requested
      if (make_executable) {
        files <- fs::dir_ls(dest_dir, recurse = TRUE, type = "file")
        fs::file_chmod(files, "u+x")
      }

      return(invisible(dest_dir))
    } else {
      # If not a zip, just copy the file to destination
      dest_path <- fs::path(dest_dir, asset$name)
      fs::file_copy(temp_path, dest_path)

      # Set execute permission if requested
      if (make_executable) {
        fs::file_chmod(dest_path, "u+x")
      }

      return(invisible(dest_path))
    }
  })
#' Wrap around a bslib tooltip
#'
#' Specify a UI element together with a description.
#'
#' @param element An arbitrary, fully specified UI element.
#' @param description The description to show as a tooltip.
#' @param placement The position of the description. Passed as argument
#' with the same name to `tooltip`. Default is "auto".
#'
#' @returns A \code{\link[bslib]{tooltip}} expression to be used in an UI element.
#'
#' @keywords internal
wrap_tooltip <- function(element, description = "", placement = "auto") {
  bslib::tooltip(
    element,
    description,
    placement = placement
  )
}
