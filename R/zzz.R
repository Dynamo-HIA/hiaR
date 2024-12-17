.onLoad <- function(libname, pkgname) {
  resources <- system.file("application/www", package = "hiaR")
  addResourcePath("www", resources)
}
