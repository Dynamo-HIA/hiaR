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
#' root <- xml2::xml_new_root('prevalences')
#' df <- data.frame(age = c(21, 21, 22, 22), sex = c(0, 1, 0, 1), value = c(1000, 2000, 1500, 3000))
#' root <- add_df_to_xml(root, df, 'prevalence')
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
#' schema <- read_xml_schema('populationsize')
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
#' root <- xml2::read_xml('<root><child>Example</child></root>')
#' validate_xml_schema(root, 'example_schema')
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
  if (length(gregexpr(pattern, s)) != count){
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


