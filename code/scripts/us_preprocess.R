# @@@@@@@@@@@@@@@@@@@@@@
# US: preprocessing-----
# @@@@@@@@@@@@@@@@@@@@@@

census_regions <- purrr::map_df(region_compass_divnumber_divname_ls, ~ as.data.frame(.x), .id = "region_compass_divnumber_divname") %>%
  rename(state = .x) %>%
  separate(region_compass_divnumber_divname, into = c("region", "compass", "divnumber", "divname"))


# get the data
df1_us_jhu <- getus_all() %>%

  # Rode Islande has not deaths at the county level:
  # https://coronavirus.jhu.edu/us-map-faq
  # we are going to remove RI
  filter(state != "Rhode Island")


# we get RI from the NYT repository
df1_us_nyt <- getus_all(repo = "nyt") %>%
  # fips 0 is for the state unassigned deaths
  filter(state == "Rhode Island")

# row bind and filter for till data freeze
df1_us <- bind_rows(df1_us_jhu, df1_us_nyt) %>%
  filter(date <= !!date_freeze)


# some cleaning
suppressWarnings(
  df2 <-
    df1_us %>%
    # calculate age65_over
    mutate(perc_age65_over = `perc_65_69` + `perc_70_74` + `perc_75_79` + `perc_80_84` + perc_85_over) %>%
    mutate(urban = if_else(urban == "Urban", 1, 0)) %>%

    mutate(total_tests = positive + negative) %>%
    # total hospital beds normalized per population
    mutate(ratio_beds = total_beds / total_pop) %>%

    # calculate day since first case
    # 0/0 generate warning
    mutate(f_date = case_when(cases >= 1 ~ date)) %>%
    group_by(fips) %>%
    mutate(f_date = min(f_date, na.rm = TRUE), days_f0 = as.numeric(date - f_date)) %>%
    ungroup() %>%
    mutate(days_f0 = if_else(is.finite(days_f0), days_f0, NA_real_)) %>%


    # perc races
    # mutate_at and mutate(across) crashes so I have to repeat code
    mutate(perc_black = total_black / total_pop * 100) %>%
    mutate(perc_white = total_white / total_pop * 100) %>%
    mutate(perc_lat = total_latino / total_pop * 100) %>%
    mutate(perc_asian = total_asian / total_pop * 100) %>%
    mutate(perc_island = total_pacific_islander / total_pop * 100) %>%
    mutate(perc_native = total_native / total_pop * 100) %>%
    mutate(perc_other = total_other_race / total_pop * 100) %>%
    mutate(perc_two_more_races = total_two_more_races / total_pop * 100) %>%


    # perc divided by 100
    mutate_at(vars(starts_with("perc")), function(x) x / 100) %>%

    # family with one parent together
    mutate(perc_family_only_onep = perc_families_only_female + perc_families_only_male) %>%

    # add Distric of Columbia to Maryland
    mutate(state = replace(state, state == "District of Columbia", "Maryland")) %>% 
    # there are a 3 counties  0 % vaccination, possibly becouse data is not available. filter them out
    filter(perc_imm65 > 0) 
)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# dat_original ORIGINAL VARIABLE (as in the preprint) -----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Function dat_select_ZZ_XX defined in analysis_functions.R
# Function that selects variables from dataframe (df2), join the dataframe with the one containing census regions,
# rename variables applying prefix XX, ZZ and calculate logit of ZZ

dat_original <- dat_select_ZZ_XX(dat = df2, var_select = to_select, census_r = census_regions)



