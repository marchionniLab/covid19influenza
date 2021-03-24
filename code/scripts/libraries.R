# Jun 4, 2020
# Baltimore, MD
# Claudio Zanettini
#
# Code of the analyses for paper xxxxx


# All the data can be retrieved using the `covid19census`s package developed by the lab
# Information regarding the package can be found at https://github.com/c1au6i0/covid19census
# The script used to import and preprocess data from the different sources, and the raw-data can be found in the package
# github repository https://github.com/c1au6i0/covid19census/blob/dev/data-raw/import_raw.R
#
# Note that the script relays heavily on the `dplyr` functions `rename_at` and `summarize_at` that
# have been recently  replaced by `across` and `where` in dplyr v1

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Load libraries and defines functions -----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Note: use package `renv` and `renv.lock` file in parent folder to install the
# same versions of packages that we used.


# # Use this to install the covid19census developmental pacakge
devtools::install_github("c1au6i0/covid19census", ref = "dev2019")

library(biostat3)
library(broom)
library(car)
library(caret)
library(covid19census)
library(drake)
library(forcats)
library(gridExtra)
library(gganimate)
library(ggridges)
library(kableExtra)
library(knitr)
library(lme4)
library(janitor)
library(lubridate)
library(oddsratio)
library(patchwork)
library(purrr)
library(RColorBrewer)
library(randomForest)
library(scales)
library(splines)
library(stringr)
library(skimr)
library(tidyverse)
library(xtable)

theme_set(theme_bw())
