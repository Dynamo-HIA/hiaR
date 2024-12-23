% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relative_risk_module.R
\name{single_relative_risk_server}
\alias{single_relative_risk_server}
\title{Server logic for a single group of relative risks}
\usage{
single_relative_risk_server(id, choices)
}
\arguments{
\item{id}{A character string. The ID that matches the corresponding UI element.}

\item{choices}{A reactive expression returning a named list of available choices.}
}
\value{
A reactive expression that returns the names
of the selected choices.
}
\description{
Server-side logic for a single group of relative risks,
handling the selection of choices from the checkbox group.
}
\keyword{internal}
