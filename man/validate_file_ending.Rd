% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{validate_file_ending}
\alias{validate_file_ending}
\title{Validate the file ending of a file}
\usage{
validate_file_ending(file_name, ending)
}
\arguments{
\item{file_name}{String of the file name.}

\item{ending}{The file name suffix, ie \code{.xml}.}
}
\value{
Throws an error if validation fails.
}
\description{
Validate the file ending of a file
}
\examples{
\dontrun{
validate_file_ending("hello.txt", ".xml") # error
validate_file_ending("hello.py", ".py") # success
}

}
\keyword{internal}
