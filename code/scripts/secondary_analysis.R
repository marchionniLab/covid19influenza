# @@@@@@@@@@@@@@@@@@@@@@@@
# Secondary---------------
# @@@@@@@@@@@@@@@@@@@@@@@@


mess <- "\nPerforming secondary analyses on original!\n"
sep_mess <- paste(rep.int("=", nchar(mess)), collapse = "")

message(paste0(sep_mess, mess, sep_mess))

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Set some general parameters and dataframe to use---------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

data_to_use <- dat_original
date_freeze <- "2020-12-14"
# dates
dates_tostudy <- sort(unique(data_to_use$date))

# variables used
variables_used <- "original"

# variables
XX <- grep("XX", names(data_to_use), value = TRUE)
XX <- XX[XX != "XX_total_beds"]

# min number of cases to be included
min_filt <- 1

form_ps <- reformulate(termlabels = XX, response = "logitZZ_perc_imm65")

# For the propensity score 
original_today_1case <- data_to_use %>%
  filter(date == max(date)) %>%
  filter(NC_cases >= min_filt)

form_ps <- reformulate(termlabels = XX, response = "logitZZ_perc_imm65")
prop_scores_original <- lm(form_ps, data = original_today_1case)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# US: Quintiles
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

pp_split_quint <- 5

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Quintile state-----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# glm_pp is the function to run the regression. Check details regarding the
# function in `scripts/function_analysis`


quint_state_original <- glm_pp(
  dat = data_to_use,
  dates_tostudy = max(dates_tostudy),
  filt_cases = min_filt,
  form = form_ps,
  offset_f = "log(NP_total_pop)",
  var_dep = "YY_deaths",
  var_int = "ZZ_perc_imm65",
  include_model = TRUE,
  verbose = TRUE,
  nation = "us",
  pp_split = pp_split_quint,
  inter = FALSE,
  adjust = c("state")
)

quint_state_original <- extract_model(quint_state_original, filt_cases = min_filt)

quint_state_original <- calculate_MRR(tidy(quint_state_original), 10) %>%
  mutate(adjustment = "state", nyc_removed = FALSE, type_pp = "quintile")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# US: Tertiles
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

pp_split_tert <- 3


# @@@@@@@@@@@@@@@@@@
# Tertile state-----
# @@@@@@@@@@@@@@@@@@

tert_state_original <- glm_pp(
  dat = data_to_use,
  dates_tostudy = max(dates_tostudy),
  filt_cases = min_filt,
  form = form_ps,
  offset_f = "log(NP_total_pop)",
  var_dep = "YY_deaths",
  var_int = "ZZ_perc_imm65",
  include_model = TRUE,
  verbose = TRUE,
  nation = "us",
  pp_split = pp_split_tert,
  inter = FALSE,
  adjust = c("state")
)

tert_state_original <- extract_model(tert_state_original, filt_cases = min_filt)

tert_state_original <- calculate_MRR(tidy(tert_state_original), 10) %>%
  mutate(adjustment = "state", nyc_removed = FALSE, type_pp = "tertile")

# @@@@@@@@@@@@@@
# US: Continuous
# @@@@@@@@@@@@@@

pp_split_cont <- 0


# @@@@@@@@@@@@@@@@@@@@@@@
# PS continous state ----
# @@@@@@@@@@@@@@@@@@@@@@@

cont_state_original <- glm_pp(
  dat = data_to_use,
  dates_tostudy = max(dates_tostudy),
  filt_cases = min_filt,
  form = form_ps,
  offset_f = "log(NP_total_pop)",
  var_dep = "YY_deaths",
  var_int = "ZZ_perc_imm65",
  include_model = TRUE,
  verbose = TRUE,
  pp_split = pp_split_cont,
  nation = "us",
  adjust = "divname"
)

cont_state_original <- extract_model(cont_state_original, filt_cases = min_filt)

cont_state_original <- calculate_MRR(tidy(cont_state_original), 10) %>%
  mutate(adjustment = "state", nyc_removed = FALSE, type_pp = "continuous")


MMR_secondary_original_binded <- bind_rows(
  quint_state_original,
  tert_state_original,
  cont_state_original
)

MMR_secondary_original <- MMR_secondary_original_binded %>%
  filter(term == "ZZ_perc_imm65") %>%
  mutate(
    date = !!date_freeze,
    variables_used = !!variables_used,
    filt = !!min_filt,
    analysis_group = "secondary"
  ) %>%
  relocate(c("date", "variables_used", "filt", "adjustment", "analysis_group", "nyc_removed", "type_pp"),
    .before = "term"
  )
