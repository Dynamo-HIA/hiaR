% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{read_xml_schema}
\alias{read_xml_schema}
\title{Read an XML Schema from the Package}
\usage{
read_xml_schema(filename)
}
\arguments{
\item{filename}{A character string representing the base name of the XML schema file (with or without the \code{.xsd} extension).}
}
\value{
An XML document object representing the parsed XML schema.
}
\description{
This function reads an XML schema (XSD file) from the \code{extdata/schemas} directory. It constructs the full file path to the schema and returns the parsed XML document.
}
\details{
The function locates the specified XML schema file within the \code{extdata/schemas} directory, optionally appends the \code{.xsd} extension to the filename, and reads the schema file using \code{xml2::read_xml}. This is useful for working with pre-defined schemas within the package's directory structure.
}
\examples{
\dontrun{
# Read an XML schema file named 'example_schema' from the package
schema <- read_xml_schema("populationsize")
}

}
\keyword{internal}
