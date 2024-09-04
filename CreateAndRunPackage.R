library(devtools)
library(usethis)
library(covr)
library(lintr)
library(funspotr)

create_package(
  path = ".",
  fields = list(
    "Authors@R" = person(
      given = "Hendriek",
      family = "Boshuizen",
      email = "hendriek.boshuizen@rivm.nl",
      role = c("aut", "cre")
    ),
    "Title" = "Dynamic Modeling for Health Impact Assessment",
    "Description" = "DYNAMO-HIA (DYNamic MOdeling for Health 
	Impact Assessment) is a generic software tool specially 
	designed to aid the quantification step in health impact 
	assessment. DYNAMO-HIA is generic in the sense that it 
	allows arbitrary chronic diseases or risk-factors that 
	fit the standard causal pathway of the health impact 
	assessment framework to be modeled. The user can import 
	and use his or her own data to add and analyze diseases 
	or populations that are of interest to him or her."
  ),
  rstudio = FALSE,
  roxygen = TRUE,
  check_name = TRUE,
  open = FALSE
)

use_readme_rmd(open = FALSE)
use_package_doc(open = FALSE)
use_gpl3_license()
use_citation()
use_testthat()
use_vignette(
  name = "user-manual",
  title = "DYNAMO-HIA User Manual"
)
use_pkgdown()
use_logo("Logo.webp")

assumed.packages <- c("dplyr", "shiny", "shinydashboard", "shinyjs", "shinyFiles", "stats")
functions.and.packages <- list_rbind(lapply(
  X = list.files(path = "R/", pattern = "[^makeTreeList].*\\.R$", full.names = TRUE), # makeTreeList.R gives errors
  FUN = spot_funs_custom,
  pkgs = assumed.packages,
  keep_in_multiple_pkgs = TRUE
))

sapply(setdiff(unique(functions.and.packages$pkgs), c("base", "(unknown)")), use_package)

package_coverage(".")
lint_package()

load_all()
document()
test()
check(error_on = "never")
install(upgrade = "never")
build_site()

hiaR::run.shiny.app(
  working.directory = "N:/2023 Dynamo-HIA/MVP/Workspace/",
  backend.directory = "N:/"
)
