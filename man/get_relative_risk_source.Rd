% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_loading_helpers.R
\name{get_relative_risk_source}
\alias{get_relative_risk_source}
\title{Get the "source" for relative risk flows}
\usage{
get_relative_risk_source(input_list, lookup_items, extract = TRUE)
}
\arguments{
\item{input_list}{A nested list containing relative risk information.}

\item{lookup_items}{A character vector of items to look up within each sublist.}

\item{extract}{Logical. If TRUE (default), extracts the source of relative risks, by calling \link{extract_relative_risk_source}.
If FALSE, returns the name of the item containing the relative risk.}
}
\value{
A nested list structure containing the extracted information or item names at the leaf nodes.
If an empty directory is the leaf node, its value is an empty list.
}
\description{
Function to extract relative risk information from a nested list structure.
It returns the filename and the source of the relative risk. The source
is extracted from the file name when \code{extract} is TRUE, and
otherwise from the item containing the relative risk information.
}
\examples{
\dontrun{
groups <- get_file_groups("../data/Tutorial_Data/Reference_Data")
keywords_disease <- c("Relative_Risks_From_Risk_Factor", "Relative_Risks_From_Diseases")
keywords_rf <- c("Relative_Risks_For_Death", "Relative_Risks_For_Disability")
get_relative_risk_source(groups$diseases, keywords_disease)
get_relative_risk_source(groups$risk_factors, keywords_rf, FALSE)
}

}
\keyword{internal}
