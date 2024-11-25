# Wrapper functions around external functions -- for mocking in tests
# See https://testthat.r-lib.org/reference/local_mocked_bindings.html

parse_dirpath_wrapper <- function(volumes, button) {
  return(shinyFiles::parseDirPath(volumes, button))
}

parse_filepath_wrapper <- function(volumes, button) {
  return(shinyFiles::parseFilePaths(volumes, button))
}
