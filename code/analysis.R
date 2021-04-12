# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# This launches all the scripts and runs all the analyses ------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

library(here)
to_run <- c(
  "libraries.R",
  "functions_analysis.R"
  # "us_preprocess.R" # instead of running this we load the data already processed
)

dat_original <- readRDS(here("data", "data_us.RDS"))

lapply(to_run, function(x) source(here("code", "scripts", x)))

lapply(list("stratified_analysis.R", "secondary_analysis.R"), function(x) source(here("code", "scripts", x)))


# Just checking Mortality Rate 
dat_original %>% 
  filter(NC_cases > 0) %>% 
  mutate(mr = YY_deaths/NP_total_pop * 100000) %>% 
  summarize(across(.cols = "mr", list(mean = mean, median =  median, iqr = IQR, sd =  sd)))
    

# @@@@@@@@@@@
# Figures ---
# @@@@@@@@@@@


# @@@
# MRR
# @@@

MRR_all_original_3 <- MRR_strata_original_3 %>%
  select(-weighted_variance) %>%
  bind_rows(MMR_secondary_original) %>%
  filter(date == ymd("2020-12-14"), filt == 1, adjustment == "state", nyc_removed == FALSE) %>% 
  filter(type_pp != "multivar") %>%
  unite(id, c("type_pp", "adjustment", "nyc_removed"), remove = FALSE)



point_width <- 0.8
p_strata_original_tert <- MRR_all_original_3 %>%
  mutate(
    type_pp = factor(type_pp, levels = c("continuous", "quintile", "tertile", "strata_3"))
    # adjustment = factor(adjustment, levels = c("state", "divname")),
    # nyc_removed = factor(as.character(nyc_removed), levels = c("TRUE", "FALSE"))
  ) %>%
  ggplot(aes(MRR, type_pp, shape = type_pp), colour = "black") +
  geom_linerange(aes(xmin = MRR_conf.low, xmax = MRR_conf.high),
                 show.legend = FALSE,
                 position = position_dodge(width = point_width)
  ) +
  scale_shape_manual(values = c(21, 22, 23, 24)) +
  # scale_fill_manual(values = c("grey", "black")) +
  geom_point(size = 4.5, position = position_dodge(width = point_width), fill = "grey") +
  scale_x_continuous(limits = c(0.8, 1
  )) +
  scale_y_discrete(labels = c(
    "strata_3" = "strata",
    "tertile" = "tertiles",
    "quintile" = "quintiles"
  )) +
  geom_vline(aes(xintercept = 1), lty = 3) +
  # geom_hline(aes(yintercept = 1.5), lty = 1, lwd = 0.3) +
  # geom_hline(aes(yintercept = 2.5), lty = 1, lwd = 0.3) +
  # geom_hline(aes(yintercept = 3.5), lty = 1, lwd = 0.3) +
  guides(fill = guide_legend(override.aes = list(shape = c(21, 22)))) +
  labs(
    x = "Mortality Risk Ratio (+/- CI)",
    y = NULL,
    fill = "",
    shape = "",
    colour = ""
  ) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 11)
  )


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Correlation-based diagnostics
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Correlation-based diagnostic see Austin 2019

# For each covariate we calculate the correlation with the exposure before and after stratification.
# We average the absolute correlation between strata (weighted average)


dat_name <- "original"
data_to_use <- get(paste0("dat_", dat_name))
XX <- grep("XX", names(data_to_use), value = TRUE)
XX <- XX[XX != "XX_total_beds"]

# This is just to reoder the variables
new_order2 <- c(
  
  "XX_days_f0",
  
  "XX_perc_families",
  "XX_perc_family_only_onep",
  "XX_perc_edu_bachelor",
  "XX_perc_withinternet",
  "XX_median_income",
  
  "XX_ratio_beds",
  "XX_perc_alzheimer_dementia",
  "XX_perc_asthma",
  "XX_perc_atrial_fibrillation",
  "XX_perc_cancer_breast",
  "XX_perc_cancer_colorectal",
  "XX_perc_cancer_lung",
  "XX_perc_ch_obstructive_pulm",
  "XX_perc_chronic_kidney_disease",
  "XX_perc_depression",
  "XX_perc_diabetes",
  "XX_perc_heart_failure",
  "XX_perc_hypertension",
  "XX_perc_ischemic_heart_disease",
  "XX_perc_obesity",
  "XX_perc_rheumatoid_arthritis",
  "XX_perc_stroke",
  "XX_perc_tobacco_use",
  
  "XX_pm2.5",
  "XX_winter_temp",
  "XX_winter_hum",
  "XX_summer_temp",
  "XX_summer_hum",
  
  "XX_median_age",
  "XX_perc_age65_over",
  "XX_sex_ratio",
  "XX_child_dependency",
  
  
  "XX_perc_black",
  "XX_perc_lat",
  "XX_perc_white",
  "XX_perc_asian",
  "XX_perc_island",
  "XX_perc_other",
  "XX_perc_two_more_races"
)

n_strata <- 3

tert_diagn <- cor_diagnostic(
  exposure = "ZZ_perc_imm65",
  confounders = XX,
  number_strata = n_strata,
  response = "logitZZ_perc_imm65",
  dat = data_to_use
)

ballance_2019 <- tert_diagn$all_cor %>% 
  filter(metric == "weighted_mean_strata") %>% 
  mutate(confounder  = factor(confounder, levels =  rev(new_order2))) %>% 
  mutate(confounder = dplyr::recode(confounder,
                                    "XX_perc_families" = "% families",
                                    "XX_perc_family_only_onep" = "% families with only one parent",
                                    "XX_perc_edu_bachelor" = "% with bachelor degree",
                                    "XX_perc_withinternet" = "% with internet",
                                    "XX_ratio_beds" = "Ratio of hospital beds",
                                    "XX_perc_alzheimer_dementia" = "% with Alzheimer dementia",
                                    "XX_perc_asthma" = "% with asthma",
                                    "XX_perc_atrial_fibrillation" = "% with atrial fibrillation",
                                    "XX_perc_cancer_breast" = "% with breast cancer",
                                    "XX_perc_cancer_colorectal" = "% with colorectal cancer",
                                    "XX_perc_cancer_lung" = "% with lung cancer",
                                    "XX_perc_ch_obstructive_pulm" = "% with chronic obstructive pulmonary disease",
                                    "XX_perc_chronic_kidney_disease" = "% with chronic kidney disease",
                                    "XX_perc_depression" = "% with depression",
                                    "XX_perc_diabetes" = "% with diabetes",
                                    "XX_perc_heart_failure" = "% with heart failure",
                                    "XX_perc_hypertension" = "% with hypertension",
                                    "XX_perc_ischemic_heart_disease" = "% with ischemic heart disease",
                                    "XX_perc_obesity" = "% with obesity",
                                    "XX_perc_rheumatoid_arthritis" = "% with rheumatoid arthritis",
                                    "XX_perc_stroke" = "% with stroke",
                                    "XX_perc_tobacco_use" = "% using tobacco",
                                    "XX_median_income" = "Median income",
                                    "XX_pm2.5" = "PM2.5",
                                    "XX_summer_temp" = "Summer temperature",
                                    "XX_summer_hum" = "Summer humidity",
                                    "XX_winter_temp" = "Winter temperature",
                                    "XX_winter_hum" = "Winter humidity",
                                    "XX_perc_age65_over" = "% age 65 and older",
                                    "XX_median_age" = "Median age",
                                    "XX_sex_ratio" = "Sex ratio",
                                    "XX_child_dependency" = "Child dependency ratio",
                                    "XX_perc_black" = "% Blacks",
                                    "XX_perc_lat" = "% Latinos",
                                    "XX_perc_white" = "% Whites",
                                    "XX_perc_asian" = "% Asians",
                                    "XX_perc_island" = "% Native Islanders",
                                    "XX_perc_other" = "% other race",
                                    "XX_perc_two_more_races" = "% two or more races",
                                    "XX_days_f0" = "Days since the first case"
  )) %>% 
  mutate(type = recode(type, "no_strata" = "before", "strata" = "after")) %>% 
  rename(Stratification = type) %>% 
  mutate(Stratification = factor(Stratification, levels = c("before", "after"))) %>% 
  ggplot(aes(value, confounder, fill = Stratification, group = Stratification, shape = Stratification)) +
  # geom_vline(xintercept = 0.1, lty = 3) +
  geom_line(orientation = "y") +
  scale_shape_manual(values = 21:22) +
  scale_fill_manual(values = c("grey", "white")) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(y = NULL,
       x = "Modulus of Correlation with Influenza Vaccination") +
  theme(legend.position = c(0.8, 0.9),
        legend.background = element_rect(size = 0.2, linetype = "solid", color = "black")
  )


combined_fig1 <- ballance_2019 + p_strata_original_tert +
  plot_annotation(
    tag_levels = "A"
  ) 


# @@@@@@@@@@@@@@@@@
# MRR in Stata ----
# @@@@@@@@@@@@@@@@

# Strata MRR and relative CI

tab_strata <- strata_states_original_dates_3 %>% 
  dplyr::select(-data, - model) %>% 
  filter(term == "ZZ_perc_imm65") %>% 
  calculate_MRR(incr = 10) %>% 
  filter(date == max(date)) %>%
  
  select(PP_split, term, MRR, MRR_conf.low, MRR_conf.high) %>% 
  mutate(term = "Vaccination Coverage")

names(tab_strata) <- c("Stratum", "Term", "MRR", "C.I. low", "C.I. high")

# @@@@@@@@@@@@@@@@@@@@
# Supplementarty -----
# @@@@@@@@@@@@@@@@@@@@


# @@@@@@@@@@@@@@@@
# Propensity Score
# @@@@@@@@@@@@@@@@

prop_scores_original %>%
  tidy() 


# @@@@@@@@@@@@@
# Strata DemTab
# @@@@@@@@@@@@@

# Demographic Table reported in supplementary material that indicates
# mean and sd of each variable in each stratum


new_order <- c(
  "n",
  "death_rate",
  "case_ratio",
  "XX_days_f0",
  
  "XX_perc_families",
  "XX_perc_family_only_onep",
  "XX_perc_edu_bachelor",
  "XX_perc_withinternet",
  "XX_median_income",
  
  
  "ZZ_perc_imm65",
  "XX_ratio_beds",
  "XX_perc_alzheimer_dementia",
  "XX_perc_asthma",
  "XX_perc_atrial_fibrillation",
  "XX_perc_cancer_breast",
  "XX_perc_cancer_colorectal",
  "XX_perc_cancer_lung",
  "XX_perc_ch_obstructive_pulm",
  "XX_perc_chronic_kidney_disease",
  "XX_perc_depression",
  "XX_perc_diabetes",
  "XX_perc_heart_failure",
  "XX_perc_hypertension",
  "XX_perc_ischemic_heart_disease",
  "XX_perc_obesity",
  "XX_perc_rheumatoid_arthritis",
  "XX_perc_stroke",
  "XX_perc_tobacco_use",
  
  "XX_pm2.5",
  "XX_winter_temp",
  "XX_winter_hum",
  "XX_summer_temp",
  "XX_summer_hum",
  
  "XX_median_age",
  "XX_perc_age65_over",
  "XX_sex_ratio",
  "XX_child_dependency",
  
  
  "XX_perc_black",
  "XX_perc_lat",
  "XX_perc_white",
  "XX_perc_asian",
  "XX_perc_island",
  "XX_perc_other",
  "XX_perc_two_more_races",
  
  "NC_cases",
  "logitZZ_perc_imm65"
)


new_order2 <- c(
  
  "XX_days_f0",
  
  "XX_perc_families",
  "XX_perc_family_only_onep",
  "XX_perc_edu_bachelor",
  "XX_perc_withinternet",
  "XX_median_income",
  
  "XX_ratio_beds",
  "XX_perc_alzheimer_dementia",
  "XX_perc_asthma",
  "XX_perc_atrial_fibrillation",
  "XX_perc_cancer_breast",
  "XX_perc_cancer_colorectal",
  "XX_perc_cancer_lung",
  "XX_perc_ch_obstructive_pulm",
  "XX_perc_chronic_kidney_disease",
  "XX_perc_depression",
  "XX_perc_diabetes",
  "XX_perc_heart_failure",
  "XX_perc_hypertension",
  "XX_perc_ischemic_heart_disease",
  "XX_perc_obesity",
  "XX_perc_rheumatoid_arthritis",
  "XX_perc_stroke",
  "XX_perc_tobacco_use",
  
  "XX_pm2.5",
  "XX_winter_temp",
  "XX_winter_hum",
  "XX_summer_temp",
  "XX_summer_hum",
  
  "XX_median_age",
  "XX_perc_age65_over",
  "XX_sex_ratio",
  "XX_child_dependency",
  
  
  "XX_perc_black",
  "XX_perc_lat",
  "XX_perc_white",
  "XX_perc_asian",
  "XX_perc_island",
  "XX_perc_other",
  "XX_perc_two_more_races"
)


# Stat in each strata
summary_strata_last_day_pp <- 
  original_pp_all_var %>% 
  mutate(XX_ratio_beds = XX_ratio_beds * 1000) %>% 
  mutate(
    death_rate = YY_deaths / NP_total_pop * 100000,
    case_ratio = NC_cases / NP_total_pop * 100000,
    XX_median_income = XX_median_income / 1000
  ) %>% 
  select(-XX_total_beds, -YY_deaths, -NP_total_pop) %>% 
  group_by(PP_split) %>% 
  mutate(n = n()) %>% 
  summarize(across(where(is.numeric), .fns = list(mean = mean, sd = sd))) %>% 
  pivot_longer(cols = NC_cases_mean:n_sd, names_to = "variable_stat") %>%
  extract("variable_stat", c("variable", "stat"), "(.*)_(.*)") %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  select(-fips_mean, -fips_sd)


# All togheter
summary_strata_last_day <- original_pp_all_var %>% 
  mutate(XX_ratio_beds = XX_ratio_beds * 1000) %>% 
  mutate(
    death_rate = YY_deaths / NP_total_pop * 100000,
    case_ratio = NC_cases / NP_total_pop * 100000,
    XX_median_income = XX_median_income / 1000
  ) %>% 
  select(-XX_total_beds, -YY_deaths, -NP_total_pop) %>% 
  mutate(PP_split = "all") %>% 
  group_by(PP_split) %>% 
  mutate(n = n()) %>% 
  summarize(across(where(is.numeric), .fns = list(mean = mean, sd = sd))) %>% 
  pivot_longer(cols = NC_cases_mean:n_sd, names_to = "variable_stat") %>%
  extract("variable_stat", c("variable", "stat"), "(.*)_(.*)") %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  select(-fips_mean, -fips_sd) %>% 
  bind_rows(summary_strata_last_day_pp)

# Some cleaning to make it look better
pp_supp <- summary_strata_last_day  %>% 
  mutate(across(.cols = c("mean", "sd"), ~ round(.x, 3))) %>% 
  mutate(mean_sd = paste0(mean, " (", sd, ")")) %>% 
  select(PP_split, variable, mean_sd) %>% 
  pivot_wider(names_from = PP_split, values_from = mean_sd) %>% 
  arrange(match(variable , !!new_order)) %>% 
  mutate(variable = recode(variable,
                           "n" = "Number of counties",
                           
                           "death_rate" = "Death rate (per 100,000 people)",
                           "case_ratio" = "Confirmed-case rate (per 100,000 people)",
                           "XX_days_f0" =  "Number of days since first case",
                           
                           
                           "XX_perc_families" = "% families",
                           "XX_perc_family_only_onep" = "% families with only 1 parent",
                           "XX_perc_edu_bachelor" = "% with bachelor or higher degree",
                           "XX_perc_withinternet" = "% with internet",
                           "XX_median_income" = "Median income ($1,000)",
                           
                           "ZZ_perc_imm65" = "% influenza vaccination coverage ", 
                           "XX_ratio_beds" = "Ratio of hospital beds",
                           "XX_perc_alzheimer_dementia" = "% with Alzheimer's disease",
                           "XX_perc_asthma" = "% with asthma",
                           "XX_perc_atrial_fibrillation" = "% with atrial fibrillation",
                           "XX_perc_cancer_breast" = "% with breast cancer",
                           "XX_perc_cancer_colorectal" = "% with colorectal cancer",
                           "XX_perc_cancer_lung" = "% with lung cancer",
                           "XX_perc_ch_obstructive_pulm" = "% with obstructive pulmonary disease",
                           "XX_perc_chronic_kidney_disease" = "% with chronic kidney disease",
                           "XX_perc_depression" = "% with depression",
                           "XX_perc_diabetes" = "% with diabetes",
                           "XX_perc_heart_failure" = "% with heart failure",
                           "XX_perc_hypertension" = "% with hypertension",
                           "XX_perc_ischemic_heart_disease" = "% with ischemic heart disease",
                           "XX_perc_obesity" = "% with obesity",
                           "XX_perc_rheumatoid_arthritis" = "% with rheumatoid arthritis",
                           "XX_perc_stroke" = "% stroke transient ischemic attack",
                           "XX_perc_tobacco_use" =  "% using tobacco",
                           
                           "XX_pm2.5" = "Average PM2.5 (microg/m3)",
                           "XX_summer_temp" = "Summer temperature (K)",
                           "XX_summer_hum" =  "% summer humidity",
                           "XX_winter_temp" = "Winter temperature (K)",
                           "XX_winter_hum" = "Winter humidity (%)",
                           
                           "XX_median_age" = "Median age",
                           "XX_perc_age65_over" = "% 65 years or more of age",
                           "XX_sex_ratio" = "Sex ratio",
                           "XX_child_dependency" = "Child dependency ratio",
                           
                           "XX_perc_black" =   "% Blacks",
                           "XX_perc_lat" =  "% Latinos",
                           "XX_perc_white" = "% Whites",
                           "XX_perc_asian" = "% Asians",
                           "XX_perc_island" = "% Native Islanders",
                           "XX_perc_other" =   "% other race",
                           "XX_perc_two_more_races" = "% two more races"
  )) %>% 
  filter(!variable %in% c("NC_cases", "logitZZ_perc_imm65", "PP")) %>% 
  rename(Variable = variable) %>% 
  mutate(Category = c("",
                      rep("COVID-19", 3),
                      rep("Socioeconomic factors", 5),
                      rep("Health factors", 19),
                      rep("Health factors", 5),
                      rep("Population demographics", 4),
                      rep("Race", 7)
  )) %>% 
  relocate(Category, .before = "Variable") %>% 
  rename(`Stratum 1` = `1`,
         `Stratum 2` = `2`,
         `Stratum 3` = `3`,
  )

# @@@@@@@@@@@@@@@@
# Correlation Plot
# @@@@@@@@@@@@@@@@


new_names <- c(
  "Death rate", 
  "Confirmed-case rate", 
  "Number of days since first case", 
  "% families",
  "% families with only 1 parent",
  "% with bachelor or higher degree",
  "% with internet",
  "Median income",
  "% influenza vaccination coverage", 
  "Ratio of hospital beds",
  "% with Alzheimer's disease",
  "% with asthma",
  "% with atrial fibrillation",
  "% with breast cancer",
  "% with colorectal cancer",
  "% with lung cancer",
  "% with obstructive pulmonary disease",
  "% with chronic kidney disease",
  "% with depression",
  "% with diabetes",
  "% with heart failure",
  "% with hypertension",
  "% with ischemic heart disease",
  "% with obesity",
  "% with rheumatoid arthritis",
  "% stroke transient ischemic attack",
  "% using tobacco",
  
  "Average PM2.5", #(\u00b5/\u33a5)
  "Summer temperature",
  "% summer humidity",
  "Winter temperature",
  "Winter humidity",
  
  "Median age",
  "% 65 years or more of age",
  "Sex ratio",
  "Child dependency ratio",
  
  "% Blacks",
  "% Latinos",
  "% Whites",
  "% Asians",
  "% Native Islanders",
  "% other race",
  "% two more races")
         

col_fun <- colorRampPalette(c("blue", "white", "red"))


# png(file = here::here("data", "corr.png"), 
#     width = 6,
#     height = 5, 
#     units = "in", 
#     res = 600)

dat_original %>% 
  filter(NC_cases > 0) %>% 
  filter(date == date_freeze) %>% 
  mutate(XX_ratio_beds = XX_ratio_beds * 1000) %>% 
  mutate(
    death_rate = YY_deaths / NP_total_pop * 100000,
    case_ratio = NC_cases / NP_total_pop * 100000,
    XX_median_income = XX_median_income / 1000
  ) %>% 
  select(any_of(new_order)) %>%
  select(-NC_cases, -logitZZ_perc_imm65) %>% 
  setNames(new_names) %>% 
  cor(method = "pearson", use = "complete.obs") %>% 
  corrplot::corrplot(
    tl.col = "black",
    tl.cex = 0.52, 
    method = "color",
    type = "full",
    order = "hclust",
    col  = col_fun(20),
    cl.length = 11,
    cl.cex = 0.5) 

# dev.off()



  



















