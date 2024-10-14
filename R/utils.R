

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
#' @seealso
#' \code{\link[xml2]{xml_add_child}} for adding child elements to XML nodes, \code{\link[xml2]{xml_new_root}} for creating new XML root nodes.
#'
#' @examples
#' \dontrun{
#' # Example for disease prevalence across age and sex groups
#' root <- xml2::xml_new_root("prevalences")
#' df <- data.frame(age = c(21, 21, 22, 22), sex = c(0, 1, 0, 1), value = c(1000, 2000, 1500, 3000))
#' root <- add_df_to_xml(root, df, "prevalence")
#' }
#'
#' @export
add_df_to_xml <- function(root, df, row_name) {
  # Iterate over each row in the data frame
  for (i in 1:nrow(df)) {
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
#' @seealso
#' \code{\link[xml2]{read_xml}} for reading and parsing XML documents.
#'
#' @examples
#' \dontrun{
#' # Read an XML schema file named "example_schema" from the package
#' schema <- read_xml_schema("populationsize")
#' }
#'
#' @export
read_xml_schema <- function(filename) {
  full_filepath <- fs::path_package(
    "extdata/schemas",
    fs::path_ext_set(filename, "xsd"),
    package = "hiaR"
    )
  return(xml2::read_xml(full_filepath))
}


create_xml_schema_error <- function(schema_name) {
  return(paste0("Validation of XML schema ", schema_name, " failed"))
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
#' @seealso
#' \code{\link[xml2]{xml_validate}} for XML validation, \code{\link{read_xml_schema}} for reading the schema.
#'
#' @examples
#' \dontrun{
#' # Validate an XML document against the "example_schema" schema
#' root <- xml2::read_xml("<root><child>Example</child></root>")
#' validate_xml_schema(root, "example_schema")
#' }
#'
#' @export
validate_xml_schema <- function(root, schema_name) {
  if(!xml2::xml_validate(root, read_xml_schema(schema_name))) {
    stop(create_xml_schema_error(schema_name))
  }
}
