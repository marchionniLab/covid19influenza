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
library(corrplot)
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

# @@@@@@@@@@@@@@@@@@@@@@@@
# Set some parameters ----
# @@@@@@@@@@@@@@@@@@@@@@@@

min_filt <- 1

# COVID-19 vaccination campain started
date_freeze <- "2020-12-14"

fips_nyc <- c(36085, 36061, 36081, 36047, 36005)

# These are the variable that will be selected. Original preprint and second preprint
to_select <- c(
  "date", "county", "state", "fips", "cases", "deaths", "total_pop", "perc_families",
  "perc_family_only_onep", "perc_edu_bachelor", "perc_withinternet", "perc_imm65", "total_beds",
  "ratio_beds", "perc_alzheimer_dementia", "perc_asthma", "perc_atrial_fibrillation",
  "perc_cancer_breast", "perc_cancer_colorectal", "perc_cancer_lung", "perc_ch_obstructive_pulm",
  "perc_chronic_kidney_disease", "perc_depression", "perc_diabetes", "perc_heart_failure",
  "perc_hypertension", "perc_ischemic_heart_disease", "perc_obesity", "perc_rheumatoid_arthritis",
  "perc_stroke", "perc_tobacco_use", "median_income", "pm2.5", "summer_temp", "summer_hum",
  "winter_temp", "winter_hum", "perc_age65_over", "median_age", "sex_ratio", "child_dependency",
  "perc_black", "perc_lat", "perc_white", "perc_asian", "perc_island", "perc_other",
  "perc_two_more_races", "days_f0", "perc_imm65"
)

to_select_2 <- c(
  "date", "county", "state", "fips", "cases", "deaths", "total_pop",
  "perc_imm65",
  ## COVID19 and state related
  "days_f0", # "pop_dens", #"total_tests",
  ## Family and Household related variables
  "perc_withinternet", # "perc_families",
  ## Socioeconomic
  "median_income",
  ## Healthcare related variables
  "annual_wellness_visit", # "ratio_beds",
  ## Education related variables
  "perc_edu_bachelor_higher",
  ## Race related variables
  "perc_black", "perc_lat", "perc_white", # "perc_asian",  "perc_native",
  # "perc_pacific_islander","perc_other_race", "perc_two_more_races",
  ## Demographic variables
  "median_age", # "perc_over65",  "child_dependency", #"sex_ratio",
  ## Medical conditions or diseases: mental health
  # "perc_alzheimer_dementia", #"perc_depression",
  ## Medical conditions or diseases: respiratory
  "perc_ch_obstructive_pulm", # "perc_asthma", # "perc_tobacco_use",
  ## Medical conditions or diseases: heart
  "perc_hypertension", # "perc_ischemic_heart_disease",
  # "perc_atrial_fibrillation",	#"perc_heart_failure",
  ## Medical conditions or diseases: cancer
  # "perc_cancer_all", #"perc_cancer_breast", "perc_cancer_colorectal", "perc_cancer_lung",
  ## Medical conditions or diseases: metabolic
  "perc_diabetes", # "perc_obesity",
  ## Medical conditions or diseases: kidney
  # "perc_chronic_kidney_disease",
  ## Medical conditions or diseases: immunological
  # "perc_rheumatoid_arthritis",
  ## Environmental variables:
  "pm2.5",
  "winter_temp", "winter_hum" # ,"summer_temp", "summer_hum"
)


region_compass_divnumber_divname_ls <- list(
  "Region1_Northeast_Division1_NewEngland" = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont"),
  "Region1_Northeast_Division2_MidAtlantic" = c("New Jersey", "New York", "Pennsylvania"),
  "Region2_Midwest_Division3_EastNorthCentral" = c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin"),
  "Region2_Midwest_Division4_WestNorthCentral" = c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"),
  "Region3_South_Division5_SouthAtlantic" = c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "District of Columbia", "West Virginia"),
  "Region3_South_Division6_EastSouthCentral" = c("Alabama", "Kentucky", "Mississippi", "Tennessee"),
  "Region3_South_Division7_WestSouthCentral" = c("Arkansas", "Louisiana", "Oklahoma", "Texas"),
  "Region4_West_Division8_Mountain" = c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming"),
  "Region4_West_Division9_Pacific" = c("Alaska", "California", "Hawaii", "Oregon", "Washington")
)

