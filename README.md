
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covid19 and influenza vaccination coverage

This repository contains the scripts used to analyze the data reported
in the paper:

**Influenza Vaccination and COVID-19 Mortality in the USA: an ecological
study**

*Claudio Zanettini\*, Mohamed Omar\*, Wikum Dinalankara, Eddie Luidy
Imada, Elizabeth Colantuoni, Giovanni Parmigiani, and Luigi Marchionni*

# Files and folders:

## /code

-   **`code/analysis.R`**: it executes all the the scripts in
    `/code/scripts/` and runs all the analyses.

## /code/script

It contains functions, and scripts used to retrieve and analyze data.

-   `code/script/libraries.R`: loads the libraries (including the one to
    retrieve data) and set some parameters.

-   `code/script/functions_analysis.R`: contains main functions used to
    analyze the data. Documentation on the arguments, and object
    returned by the functions is reported in the file.

-   `code/scripts/us_preprocess.R`: this is used to obtain U.S data
    (using the R package `covid19census`) and preprocess them.

-   `code/stratified_analysis`: it contains code for the stratified
    analysis model.

-   `code/secondary_analysis`: it contains code for the secondary
    analysis.

## /data

-   `data_us.RDS`: a static copy of the dataframe used for the analysis.

# Libraries used and versions

The `renc.lock` file lists packages and relative version used in the
project. Check `renv`
[vignette](https://rstudio.github.io/renv/articles/renv.html) for
instructions on how to restore the package environment used for the
analyses.

------------------------------------------------------------------------

# Sources and data

Details regarding the data sources as well as functions to extract
updated COVID-19 data and aggregate them with other socio-economic and
health related metrics can be found in the [covid19census R
package](https://github.com/c1au6i0/covid19census). Please refer to the
package README or documentation for more information regarding the
variables.

The file `data/data.Rdata` contains a static copy of the data used for
the analysis.
