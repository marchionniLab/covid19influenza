# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Stratified analyses---------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


mess <- "\nAnalyzing Strata Original!\n"
sep_mess <- paste(rep.int("=", nchar(mess)), collapse = "")

message(paste0(sep_mess, mess, sep_mess))

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Set some general parameters and dataframe to use---------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

data_to_use <- dat_original


dates_tostudy <- "2020-12-14"
to_filt_cases <- 1
XX <- grep("XX", names(data_to_use), value = TRUE)
XX <- XX[XX != "XX_total_beds"]

variables_used <- "original"

# Dry? no, definitely WET :(

# @@@@@@@@@@@@@@@@@@@@@@@
# TERTILE ---------------
# @@@@@@@@@@@@@@@@@@@@@@@

number_strata_tertiles <- 3 # tertiles


# @@@@@@@@@@@@@@@
# state by date
# @@@@@@@@@@@@@@

# Propensity score formula
form_ps <- reformulate(termlabels = XX, response = "logitZZ_perc_imm65")

# Regression formula
form_state <- as.formula(YY_deaths ~ ZZ_perc_imm65 + PP + state)


# strata_glm_pp is the function to run the regression. Check details regarding the
# function in `scripts/function_analysis`

strata_states_original_dates_3 <-
  strata_glm_pp(dat = data_to_use,
                filt_c = 1,
                form_ps = form_ps,
                form_glm = form_state,
                number_strata = number_strata_tertiles,
                date_ts = "2020-12-14"
                  ) %>%
  mutate(analysis = "state_dates")



# @@@@@@@@@@@@@@@@@
# Calculate MRR
# @@@@@@@@@@@@@@@@@


strata_original_results_splitted_3 <- strata_states_original_dates_3 %>%
  unite("date_filt_analysis", c("date", "filt_cases_c", "analysis")) %>%
  select(-model, -data) %>%
  split.data.frame(., .$date_filt_analysis)


# strata_weights_MRR is the function to calculate weighted MRR. Check details regarding the
# function in `scripts/function_analysis`

MRR_strata_original_3 <- map_dfr(strata_original_results_splitted_3, strata_weights_MRR) %>%
  relocate(c("date_filt_analysis", "term"), .before = estimate) %>%
  separate(date_filt_analysis, into = c("date", "filt", "adjustment", "analysis_group"), sep = "_") %>%
  mutate(filt = as.numeric(filt)) %>%
  mutate(nyc_removed = if_else(analysis_group == "nonyc", TRUE, FALSE)) %>%
  relocate(nyc_removed, .before = term) %>%
  mutate(variables_used = !!variables_used) %>%
  relocate(variables_used, .after = date) %>%
  relocate(type_pp, .before = term)


## The last date with one case
MRR_strata_original_3_today <- MRR_strata_original_3 %>%
  filter(date == max(date)) %>%
  filter(filt == 1)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# tab used to check difference in strata -----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


summ_imm65 <-  function(dat){
  dat %>% 
    summarize(
      mean_imm65 = mean(ZZ_perc_imm65), 
      sd_imm65 = sd(ZZ_perc_imm65), 
      median_imm65 = median(ZZ_perc_imm65),
      min_imm65 = min(ZZ_perc_imm65),
      max_imm65 = max(ZZ_perc_imm65))
}

divname_check <-  function(dat){
  mess <- paste0("Working on ", max(dat$date))
  message(mess)
  dat %>% 
    group_by(divname) %>% 
    summarize(n = n(), .groups = "drop_last") %>% 
    pivot_wider(names_from = divname, values_from = n)
  
}

flu_strata_summary <- strata_states_original_dates_3 %>% 
  filter(term == "ZZ_perc_imm65") %>% 
  mutate(fit = map(data, summ_imm65)) %>% 
  unnest(fit) %>% 
  select(date, PP_split, mean_imm65:max_imm65)

original_pp_all_var <- strata_states_original_dates_3 %>% 
  filter(date == max(date), term == "ZZ_perc_imm65") %>% 
  select(PP_split, data) %>% 
  unnest(data) 

divname_strata_summary <- strata_states_original_dates_3 %>% 
  filter(term == "ZZ_perc_imm65") %>% 
  mutate(fit = map(data, divname_check)) %>% 
  unnest(fit)  %>% 
  select(date, PP_split, EastNorthCentral:WestSouthCentral) 


