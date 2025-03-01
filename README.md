
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Dynamic Modeling of Health Impact Assessment in R

<!-- badges: start -->

[![R-CMD-check](https://github.com/Dynamo-HIA/hiaR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Dynamo-HIA/hiaR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Dynamo-HIA/hiaR/graph/badge.svg)](https://app.codecov.io/gh/Dynamo-HIA/hiaR)
<!-- badges: end -->

The goal of the `hiaR` R package is to provide a user-friendly interface
to the DYNAMO-HIA software tool. It contains R functions to configure
and run DYNAMO-HIA, and to visualize the results. The package also
contains a Shiny app to interactively run DYNAMO-HIA.

DYNAMO-HIA (DYNamic MOdeling for Health Impact Assessment) is a generic
software tool specially designed to aid the quantification step in
health impact assessment. DYNAMO-HIA is generic in the sense that it
allows arbitrary chronic diseases or risk-factors that fit the standard
causal pathway of the health impact assessment framework to be modeled.
The user can import and use his or her own data to add and analyze
diseases or populations that are of interest to him or her.

## Getting Started

### Running the Shiny App

You can start the Shiny app with `hiaR::launch_application`:

``` r
hiaR::launch_application()
```

### Running DYNAMO-HIA from R

You can run DYNAMO-HIA from R using the `hiaR::run_dynamo_hia` function.
This function requires a specific folder structure, reference data, and
simulation configuration file to be present. See the [`hiaR`
documentation](https://dynamo-hia.github.io/hiaR/docs/articles/example.html)
for a detailed example.

## Installation

You can install the `hiaR` package from GitHub via `remotes`:

``` r
remotes::install_github("Dynamo-HIA/hiaR")
```

## User Documentation

The documentation of the `hiaR` package is available at
<https://dynamo-hia.github.io/hiaR/>.

The documentation of DYNAMO-HIA including the user manual, examples, and
reference datasets is available at <https://dynamo-hia.github.io/>.

## Development Documentation

Make sure you have the right settings for line endings. If you open the
project in RStudio, the settings are automatically set. Otherwise, you
can set them manually:

**RStudio**

1.  Open `Tools` $\rightarrow$ `Project Options` $\rightarrow$
    `Code Editing`
2.  Tick `Ensure that source files end with newline`
3.  Set `Line ending conversions to Windows (CR/LF)`

**Console**

- Run `usethis::use_rstudio(line_ending = "windows")`

On Windows, make sure that `git config core.autocrlf` is set to `false`.

### Conventions

We follow the [tidyverse](https://style.tidyverse.org/) style guide for
R code. You can use the `styler` package to automatically format your
code:

``` r
# Style single R file (has tidyverse style as default)
styler::style_file("R/myfile.R")

# Apply style to entire package
styler::style_pkg()
```

#### Naming of Modules

For R Shiny modules, use the following naming pattern:

- The R file with the module should be named `my_purpose_module.R`
- Inside `my_purpose_module.R`, define the functions `my_purpose_ui` for
  the user interface and `my_purpose_server` for the server
- If possible, add automatic tests for the server functionality. The UI
  functionality is more complex to test and we decided to test this
  manually for now.

#### Imports

Any functions from `shiny` and `shinyFiles` can be used directly without
the `shiny::` prefix. For any other packages, be explicit and use
`mypackage::myfunction()`.

#### Before Making a Release

Before making a release, please change the default value for the
`release_tag` argument in the `download_github_release` function to the
tag of the latest release of
[DYNAMO-HIA](https://github.com/Dynamo-HIA/dynamo-hia) if necessary.

## Credits

The `hiaR` package is developed by the Netherlands eScience Center and
the Rijksinstituut voor Volksgezondheid en Milieu (RIVM; National
Institute for Public Health and the Environment).

## Generative AI Statement

The code in this repository has been partly generated and/or refined
using ChatGPT 4, Claude 3.5 Sonnet, and/or GitHub Copilot Individual.
All AI-generated output has been verified for correctness, accuracy, and
completeness, adapted where needed, and approved by the authors.
