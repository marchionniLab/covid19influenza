#' logit
#'
#' @param p numeric vector
#'
#' @return logit
logit <- function(p) {
  log(p) - log(1 - p)
}


#' dat_select_ZZ_XX
#'
#' Function that select variables from dataframe (df2), join the dataframe with the one containing census regions,
#' rename variables applying prefix XX, ZZ, remove county with 0 vaccination ciand calculate logit of ZZ
#'
#' @param dat
#' @param var_select
#' @param census_r
#'
#' @return dataframe with columns with prefix XX ZZ and with census_regions
dat_select_ZZ_XX <- function(dat, var_select, census_r = census_regions) {
  dat %>%

    # select variables
    dplyr::select(!!var_select) %>%

    # we remove NAs
    na.omit() %>%
    # filter(cases >= 1) %>%
    # make variables numeric
    mutate_at(
      vars(
        -date, -county, -state, -fips
      ),
      as.numeric
    ) %>%
    # rename
    # add XX_ to all the variables excepts those
    rename_at(
      vars(
        -date, -county, -state, -fips, -cases, -deaths,
        -perc_imm65, -cases, -total_pop
      ),
      ~ paste0("XX_", .)
    ) %>%
    rename(ZZ_perc_imm65 = perc_imm65) %>%
    rename_at(vars(deaths), ~ paste0("YY_", .)) %>%
    rename_at(vars(cases), ~ paste0("NC_", .)) %>%
    rename_at(vars(total_pop), ~ paste0("NP_", .)) %>%
    # calculate logitZZ
    # there are a 3 counties  0 % vaccination, possibly becouse data is not available. filter them out
    filter(ZZ_perc_imm65 > 0) %>% 
    mutate(logitZZ_perc_imm65 = logit(ZZ_perc_imm65)) %>%
    inner_join(census_r, by = "state")
}


#' get_PPScore
#'
#' Calculate propensity score of variable of interest, `var_int` X confounders, and glm quasipoisson(link = "log")
#' model  with `var_dep` as response and `var_int` + propensity score as factors, and `offset_f`
#' as offset
#'
#' @param dat dataframe (need to have specific shape and names)
#' @param filt_cases threshold of number of cases x county to be included in the analysis
#' @param form  formula to use for propensity score, this is a `reformulate` expression
#' @param offset_f the offset  "log(NC_cases)"
#' @param var_dep dependent variable
#' @param var_int variable of interest (for example influenza)
#' @param include_model include the glm object in the dataframe
#' @param verbose boolean
#' @param pp_split integer, the number of intervals in which to split the PP score. If 0 then continuous PP, 3 is tertiles, 5 is quintiles.
#' @param pp_engine model used to calculate propensity scores. One of "lm" or "rf". When using `randomForest`, it uses tuneRF to find optimal mtry (ntrees = 500).
#' @param nation "us" for us dataset. Return object changes depending on the nation (italy does not have counties)
#' @param inter boolean, calculates interaction of var of inter and PP score
#' @param other_a if  "only_quint" it calculate only the interaction for a model in var_dep ~ var_int:as.factor(PP) where PP is
#'     in quintile. The "only_quint" and "PP_main" are experimental features, the output cannot be extracted with extract_model.
#'     pp_split needs to be 5 for "quint only"
#' @param adjust NULL or a vector with variables for adjustment
#' @param spline if TRUE smooth the PP, ns(PP, 2)
#' @return dataframe of results. Each raw contains is a separate linear regression analysis:
#'  Most columns are self-explantory, `model` and `model_PP` are  glm objects

get_PPScore <- function(
                        dat = .x,
                        filt_cases = 1,
                        form = form_ps,
                        offset_f = "log(NC_cases)",
                        var_dep = "YY_deaths",
                        var_int = "ZZ_perc_imm65",
                        pp_split = 0,
                        pp_engine = "lm",
                        include_model = FALSE,
                        verbose = TRUE,
                        nation = "us",
                        inter = FALSE,
                        other_a = FALSE,
                        adjust = NULL,
                        spline = FALSE) {

  # Check arguments

  if (other_a == "quint_only" && pp_split != 5) {
    stop("Interaction quint_only is only avaliable when pp_split = 5")
  }

  if (spline == TRUE && pp_split != 0) {
    stop("You cannot smooth the PP if your not using continous PP!")
  }

  if (!pp_split %in% 0:100) {
    stop("Argument pp_split needs to be 0 or a integer!")
  }

  if (!inter %in% c(TRUE, FALSE)) {
    stop("Argument inter can only be TRUE or FALSE!")
  }

  dat <- dat %>%
    filter(dat$NC_cases >= !!filt_cases)

  # So you can apply  spline only when calculating continuous PP
  PP_var <- "PP"

  if (pp_split == 0 && spline == TRUE) {
    PP_var <- "ns(PP, 2)"
  }

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Calculate propensity score
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@

  if (pp_engine == "lm") {
    propensity_score_model <- lm(form, data = dat)
    propensity_score_values <- fitted.values(propensity_score_model)
  }

  if (pp_engine == "rf") {
    # tuneRF doesn't like formulas so we do that
    # form_as_character <- as.character(form)
    # vat_int_ps <- form_as_character[2]
    # other_vars <- unlist(str_split(form_as_character[3], "\\s\\+\\s"))
    #
    # y_ps <- dat[[vat_int_ps]]
    # x_ps <- as.data.frame(dat[, other_vars])
    #
    message("Searching in the forest!...")
    # propensity_score_model <- tuneRF(x = x_ps, y = y_ps, ntreeTry = 500, mtryStart = 1, improve = 0.01, doBest = T)
    # propensity_score_values <-  predict(propensity_score_model, data = dat)

    propensity_score_model <- randomForest(form_ps, data = dat_s, ntree = 300, mtry = 8)
    propensity_score_values <- predict(propensity_score_model, data = dat)
  }

  if (pp_split > 0) {
    dat <- dat %>%
      # as factor here
      mutate(PP = as.factor(ntile(propensity_score_values, !!pp_split)))
  } else {
    dat[, "PP"] <- propensity_score_values
  }


  # this create the formula that will be used in the analysis
  if (inter == TRUE) {
    sign_formula <- " * "
  }

  if (inter == FALSE) {
    sign_formula <- " + "
  }


  if (is.null(adjust)) {
    form_glm <- as.formula(paste(var_dep, "~", var_int, sign_formula, PP_var))
  }

  if (!is.null(adjust)) {
    if (length(adjust) > 1) {
      form_glm <- as.formula(paste(var_dep, "~", var_int, sign_formula, PP_var, " + ", paste0(adjust, collapse = " + ")))
    }

    if (length(adjust) == 1) {
      form_glm <- as.formula(paste(var_dep, "~", var_int, sign_formula, PP_var, " + ", adjust))
    }
  }


  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # These are just some very specific analyses
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  if (other_a == "quint_only" && pp_split == 5) {
    form_glm <- as.formula(paste(var_dep, "~ ", var_int, ":", PP_var))
  }

  if (other_a == "PP_main" && pp_split == 5) {
    form_glm <- as.formula(paste(var_dep, "~ ", "PP + ", var_int, ":", PP_var))
  }


  # @@@@@@@@@@@@@@
  # Linear Model#
  # @@@@@@@@@@@@@@
  outcome_PP <-
    # we add PP on the fly to the dataframe
    dat %>%
    glm(form_glm,
      family = quasipoisson(link = "log"),
      offset = eval(parse(text = offset_f)),
      data = .
    )


  # if we are looking only interactions with quintiles we can't filter them out in
  # the dataframe
  if (other_a %in% c("quint_only", "PP_main")) {
    # conf.int true crashes with state
    res <- tidy(outcome_PP) %>%
      mutate(
        conf.low = estimate - std.error * 1.96,
        conf.high = estimate + std.error * 1.96
      )
  } else {
    res <- tidy(outcome_PP) %>%
      filter(term == !!var_int) %>%
      mutate(
        conf.low = estimate - std.error * 1.96,
        conf.high = estimate + std.error * 1.96
      )
  }

  res <- res %>%
    mutate(
      deaths = sum(dat$YY_deaths),
      cases = sum(dat$NC_cases),
      formula = deparse(formula(outcome_PP)),
      offset_f = offset_f,
      pp_engine = pp_engine,
      pp_split = pp_split,
      spline = spline
    )


  if (nation == "us") {
    res <-
      res %>%
      mutate(
        n_counties = nrow(dat)
      )
  }

  if (include_model == TRUE) {
    res <- res %>%
      mutate(model = list(outcome_PP)) %>%
      mutate(pp_model = list(propensity_score_model))
  }

  if (verbose == TRUE) {
    message(
      cat(paste0(
        "\nDate ",
        first(dat$date2),
        ":\nApplying model ",
        first(res$formula),
        "\noffset: ",
        first(offset_f),
        "\nfilt_cases = ",
        filt_cases,
        "!"
      ))
    )
  }
  res
}


#' glm_pp
#'
#' Given a `covid-19` dataframe, apply the propensity glm analysis to subset of dates, varying inclusion criteria per county/region
#'
#' @param dat dataframe
#' @param filt_cases numeric vector of thresholds of cases X county  (default = 1)
#' @param form  formula to use for propensity score, this is a `reformulate` expression
#' @param offset_f the offset, es "log(NC_cases)"
#' @param var_dep dependent variable, es. "YY_death"
#' @param var_int indipendent variable, es ZZ_perc_imm65"
#' @param include_model include a glm in the dataframe
#' @param pp_engine model used to calculate propensity scores. One of "lm" or "rf". When using rf
#' @param verbose boolean
#' @param inter boolean, calculates interaction of var of inter and PP score
#' @param other_a if  "only_quint" it calculate only the interaction for a model in var_dep ~ var_int:as.factor(PP) where PP is
#'     in quintile. The "only_quint" and "PP_main" are experimental features, the output cannot be extracted with extract_model.
#' @param adjust variable to adjust for. If NULL no adjustment
#' @param spline if TRUE smooth the PP, ns(PP, 2)
#' @return dataframe
glm_pp <- function(dat,
                   dates_tostudy = dates_tostudy,
                   filt_cases,
                   offset_f = "log(NC_cases)",
                   var_dep,
                   var_int,
                   include_model = FALSE,
                   verbose = TRUE,
                   nation = "us",
                   pp_engine = "lm",
                   pp_split = FALSE,
                   inter = FALSE,
                   other_a = FALSE,
                   adjust = NULL,
                   form_ps = form_ps,
                   spline = FALSE,
                   ...) {
  suppressWarnings(
    zz_term <-
      dat %>%
      # filter(YY_deaths > 0) %>%
      filter(date %in% dates_tostudy) %>%
      mutate(date2 = date) %>%
      nest(-date) %>%
      # we repeat the date slice X number of case threeshold that we want to study
      slice(rep(1:n(), each = length(filt_cases))) %>%
      mutate(filt_cases_c = rep(filt_cases, nrow(.) / length(filt_cases))) %>%

      mutate(
        fit =
          map2(
            data,
            filt_cases_c,
            get_PPScore,
            form = !!form_ps,
            offset_f = !!offset_f,
            var_int = !!var_int,
            var_dep = !!var_dep,
            nation = !!nation,
            include_model = !!include_model,
            verbose = !!verbose,
            pp_engine = !!pp_engine,
            pp_split = !!pp_split,
            inter = !!inter,
            other_a = !!other_a,
            adjust = !!adjust,
            spline = !!spline
          )
      ) %>%
      unnest(fit)
  )

  zz_term_nd <- zz_term %>%
    dplyr::select(-data)

  zz_term_nd
}


#' extract_model
#'
#' @param x object return by glm_pp
#' @param fill_cases extract the model with that criterion
#' @param date date  model
#' @param model "pp_model" to extract propensity score, "main" for the main model
#'
#' @return most recent model with filt cases = 1
extract_model <- function(x, filt_cases = 1, model = "main") {
  if (model == "main") {
    mod_extr <- "model"
  }

  if (model == "pp_model") {
    mod_extr <- model
  }

  mod <- x %>%
    filter(date == max(date), filt_cases_c == !!filt_cases) %>%
    dplyr::select(!!mod_extr) %>%
    flatten()

  mod[[1]]
}


#' extract_tidy_results
#'
#' Extract tidy summary of last day available regarding var on interest  from object returned by
#' glm_pp
#'
#' @param x  object return by glm_pp
#' @param filt_cases case threshold
#' @return dataframe tidy summary
#'
#' @examples
extract_tidy_results <- function(x, filt_cases) {
  mod <- extract_model(x, filt_cases = filt_cases)
  dat_f <- tidy(mod, conf.int = TRUE)[2, ]
  dat_f[, "degrees of freedom"] <- mod$df.residual

  dat_f
}

#' Calculate MRR
#'
#' @param dat a tidy summary of a `glm` or a dataframe with columns estimate, conf.low and conf.high
#' @param incr percent increase in MRR, if 100 no change and MRR is equal to exp of the estimate
#'
#' @return dataframe
calculate_MRR <- function(dat, incr) {
  dat %>%
    mutate(
      conf.low = estimate - std.error * 1.96,
      conf.high = estimate + std.error * 1.96,
      incr = incr,
      ratio_incr = 100 / incr,
      MRR = exp(estimate / ratio_incr),
      MRR_conf.low = exp(conf.low / ratio_incr),
      MRR_conf.high = exp(conf.high / ratio_incr)
    )
}

#' extract_mortality_ratio
#'
#' Extract mortality ratios from last day of aggregated models
#'
#' @param model_pp model as return by `glm pp`
#' @param incr percent increase in MRR, if 100 no change and MRR is equal to exp of the estimate
#' @param filt_cases used to extract model with that filtering threshold.
#'
#' @return dataframe as returned by `oddratio::or_glm()`
#' @examples
extract_mortality_ratio <- function(model_pp,
                                    incr,
                                    filt_cases) {
  model_today <- extract_model(model_pp, filt_cases = filt_cases)
  calculate_MRR(tidy(model_today))
}



#' strata_glm_pp
#'
#' @param dat dataframe
#' @param date_ts  dates
#' @param filt_c  filtering criterion
#' @param pp_engine model used to calculate propensity scores. One of "lm" or "rf". When using `randomForest`, it uses tuneRF to find optimal mtry (ntrees = 500).
#' @param form_ps  formula PP score
#' @param form_glm formula glm
#' @param nonyc  boolean to exclude NYC
#' @param number_strata number of strata at which to divide the data (default is 5 for quintiles)
#' @param out if "model" returns a dataframe with results of stratified analysys. If "data" returns the dataframe
#'        with all  the observations and a column `PP`for the propensity score and `PP_split` that indicates the strata.
#'
#' @return
#' @export
#'
#' @examples
strata_glm_pp <- function(dat,
                          date_ts,
                          filt_c,
                          pp_engine = "lm",
                          form_ps,
                          form_glm,
                          nonyc = FALSE,
                          number_strata = 5,
                          include_model_data = TRUE,
                          out = "model") {
  dat <- dat %>%
    filter(date == !!date_ts) %>%
    filter(NC_cases >= !!filt_c)

  if (nonyc == TRUE) {
    dat <- dat %>%
      filter(!fips %in% !!fips_nyc)
  }

  if (pp_engine == "lm") {
    propensity_score_model <- lm(form_ps, data = dat)
    propensity_score_values <- fitted.values(propensity_score_model)
  }

  if (pp_engine == "rf") {
    # tuneRF doesn't like formulas so we do that
    # form_as_character <- as.character(form_ps)
    # vat_int_ps <- form_as_character[2]
    # other_vars <- unlist(str_split(form_as_character[3], "\\s\\+\\s"))
    #
    # y_ps <- dat[[vat_int_ps]]
    # x_ps <- as.data.frame(dat[, other_vars])
    #
    # message("Searching in the forest...")
    # propensity_score_model <- tuneRF(x = x_ps, y = y_ps, ntreeTry = 500, mtryStart = 1, improve = 0.01, doBest = T, plot = FALSE)
    # propensity_score_values <- predict(propensity_score_model, data = dat)

    propensity_score_model <- randomForest(form_ps, data = dat, ntree = 300, mtry = 8)
    propensity_score_values <- predict(propensity_score_model, data = dat)
  }

  dat_split <- dat %>%
    mutate(
      PP_split = as.factor(ntile(propensity_score_values, !!number_strata)),
      PP = propensity_score_values
    )
  if (out == "data") {
    return(dat_split)
  }

  if (out == "model") {
    suppressWarnings(
      result_model <- dat_split %>%
        nest(-PP_split) %>%
        arrange(PP_split) %>%
        mutate(
          model = map(data, ~ glm(
            form_glm,
            family = quasipoisson(link = "log"),
            offset = log(NP_total_pop), data = .x
          )),
          tidy = map(model, tidy)
        ) %>%
        unnest(tidy) %>%
        mutate(
          date = date_ts,
          number_strata = !!number_strata,
          filt_cases_c = filt_c,
        ) %>%

        relocate(date, .before = PP_split)
    )

    message(paste0("Date ", date_ts, " with filt_c ", filt_c, " analyzed!"))

    if (include_model_data == FALSE) {
      result_model <- result_model %>%
        select(-model, -data)
    }

    return(result_model)
  }
}


#' strata_weights_MRR
#'
#' take the results of `strata_glm` and calculate weighted estimate and MRR
#'
#' @param dat result of `strata_glm`
#' @param incr percent increase in MRR, if 100 no change and MRR is equal to exp of the estimate
#' @param var_int variable of interest
#'
#' @return
#' @export

strata_weights_MRR <- function(dat, incr = 10, var_int = "ZZ_perc_imm65") {
  results <- dat %>%
    filter(term == !!var_int) %>%
    # v_i is the  standard error squared
    # est_i = the quintile specific coefficient and v_i = the standard error squared.
    mutate(v_i = (std.error)^2) %>%
    # s sum of inverse variance
    # Define s = sum(i = 1 to 5) 1 / v_i, this is the sum of the inverse variance

    mutate(s = sum(1 / v_i)) %>%
    # Define w_i = (1 / v_i) / s, this is weight
    mutate(w_i = (1 / v_i) / s) %>%
    # Weighted estimator is sum(i = 1 to 5) w_i * est_i
    mutate(w_e_i = w_i * estimate) %>%
    summarise(
      estimate = sum(w_e_i),
      weighted_variance = first(s),
      std.error = sqrt(1 / weighted_variance),
      conf.low = estimate - std.error * 1.96,
      conf.high = estimate + std.error * 1.96
    ) %>%
    calculate_MRR(incr = incr)

  if ("date_filt_analysis" %in% names(dat)) {
    results <- results %>%
      mutate(date_filt_analysis = unique(dat$date_filt_analysis)) %>%
      mutate(type_pp = paste0("strata_", unique(dat$number_strata))) %>%
      mutate(term = !!var_int)
  }

  results
}


#' strata_check_pp
#'
#' Calculate the propensity score by strata (used to check what counties we are dropping)
#'
#' @param dat dataframe
#' @param date  dates
#' @param filt_c  filtering criterion
#' @param pp_engine model used to calculate propensity scores. One of "lm" or "rf". When using `randomForest`, it uses tuneRF to find optimal mtry (ntrees = 500).
#' @param form_ps  formula PP score
#' @param form_glm formula glm
#' @param nonyc  boolean to exclude NYC
#' @param number_strata number of strata at which to divide the data (default is 5 for quintiles)
#'
#' @return
#' @export
#'
#' @examples
strata_check_pp <- function(dat,
                            date_ts,
                            filt_c,
                            pp_engine = "lm",
                            form_ps,
                            nonyc = FALSE,
                            number_strata = 3) {
  dat <- dat %>%
    filter(date == !!date_ts) %>%
    filter(NC_cases >= !!filt_c)

  if (nonyc == TRUE) {
    dat <- dat %>%
      filter(!fips %in% !!fips_nyc)
  }

  if (pp_engine == "lm") {
    propensity_score_model <- lm(form_ps, data = dat)
    propensity_score_values <- fitted.values(propensity_score_model)
  }

  if (pp_engine == "rf") {
    # tuneRF doesn't like formulas so we do that
    # form_as_character <- as.character(form_ps)
    # vat_int_ps <- form_as_character[2]
    # other_vars <- unlist(str_split(form_as_character[3], "\\s\\+\\s"))
    #
    # y_ps <- dat[[vat_int_ps]]
    # x_ps <- as.data.frame(dat[, other_vars])
    # propensity_score_model <- tuneRF(x = x_ps, y = y_ps, ntreeTry = 500, mtryStart = 1, improve = 0.01, doBest = T)
    # propensity_score_values <- predict(propensity_score_model, data = dat)

    message("Searching in the forest...")

    propensity_score_model <- randomForest(form_ps, data = dat, ntree = 300, mtry = 8)
    propensity_score_values <- predict(propensity_score_model, data = dat)
  }

  dat_split <- dat %>%
    mutate(
      PP_split = as.factor(ntile(propensity_score_values, !!number_strata)),
      PP = propensity_score_values
    ) %>%
    # select(fips, PP_split, PP, ZZ_perc_imm65) %>%
    mutate(filt_c = !!filt_c)


  message(paste0("Date ", date_ts, " with filt_c ", filt_c, " analyzed!"))
  dat_split
}


#' strata_diff_summary
#'
#' Given 2 pr more  dataframes with columns "fips", "PP_split" "PP", anti_join (to check which drop), and either return the anti_join dataframe or
#' summary of the number of county per strata. The column steps indicate which dataframe  positions are compared (e.s: 1_to_2. first and second)
#'
#' @param dat list of dataframe
#' @param pos  position of the 2 contiguous dataframe to compare. if 1 it will compare the first in the list with the second in the list.
#' @param out  "summary" or "all"
strata_diff_summary <- function(dat, pos, out) {
  tot_counties <- nrow(dat[[pos]])

  all_diff <- anti_join(dat[[pos]], dat[[pos + 1]], by = "fips") %>%
    mutate(step = paste0(pos, "_to_", pos + 1))

  summary_diff <-
    all_diff %>%
    mutate(tot_counties_dropped = n(), PP_split = factor(PP_split, levels = 1:3)) %>%
    group_by(PP_split, .drop = FALSE) %>%
    summarize(n = n(), .groups = "drop_last", tot_counties = !!tot_counties) %>%
    mutate(step = paste0(pos, "_to_", pos + 1), step_num = pos + 1) %>%
    group_by(step) %>%
    mutate(tot_counties_dropped = sum(n)) %>%
    relocate(tot_counties_dropped, .before = tot_counties)


  if (out == "summary") {
    return(summary_diff)
  }

  if (out == "all") {
    return(all_diff)
  }
}



#' correlation exposure confounder
#'
#' Calculate correlation between exposure and confounders before and after stratification
#'
#' @param exposure a numeric col
#' @param confounder a col
#' @param dat a dataframe
#'
#' @return a dataframe with 4 columns: exposure, confounder, cor and n_counties

cor_conf <- function(exposure, confounder, dat) {
  data.frame(
    exposure = exposure,
    confounder = confounder,
    cor = as.numeric(cor(dat[, exposure], dat[, confounder])),
    n_counties = nrow(dat)
  )
}

#' correlation diagnostic
#'
#' Calculate correlations between confounders and exposure before and after stratification by PS.
#' The function `strata_glm_pp` is used to calculate PS scores and create strata. The function `cor_conf` to
#' calculate the correlations. This is based on the description provide in Austin 2019
#'
#' @param exposure an atomic vector indicating the name of the column in the dataframe containing the exposure
#' @param confounders a vector of col names indicating the confounders
#' @param n_strata number of strata to divide the PS into
#' @param response the name of the column indicating the response (exposure) for PS calculation. This can be different
#'   than exposure if for example using logit
#' @param dat a dataframe as returned after running us_preprocess.R
#' @param date Date
#'
#' @return a list of dataframes:
#'      1. dat_cor_crude: contains correlations exposure confounders before stratification
#'      2. dat_cor_strata_all: contains correlations in each strata
#'      3. all_cor: correlation before and after stratification.
#'      Note than the column `metric` reports the metric used to summarize the correlation over the
#'      strata. For correlation calculated before stratification (`type` no_strata), no actual summarizing metric
#'      is used. The column metric is just very convinient to make plots. Note that absolute correlation is
#'      reported in all_cor
#'
cor_diagnostic <- function(exposure,
                           confounders,
                           number_strata,
                           response,
                           dat,
                           date) {
  form_ps <- reformulate(termlabels = XX, response = "logitZZ_perc_imm65")

  if(missing(date)) date = max(dat$date)
  
  dat_last_1c <- strata_glm_pp(
    dat = dat,
    date_ts = date,
    form_ps = form_ps,
    form_glm = NULL, # this is not necessary (we are not going to calculate the glm)
    number_strata = number_strata,
    filt_c = 1,
    nonyc = FALSE,
    out = "data"
  )

  # @@@@@@
  # Before
  # @@@@@@

  dat_cor_crude <- map_dfr(XX, cor_conf, exposure = "ZZ_perc_imm65", dat_last_1c)

  # @@@@@@
  # Strata
  # @@@@@@

  dat_splitted <- split(dat_last_1c, dat_last_1c$PP_split)
  dat_cor_strata_all <- map2_dfr(rep(list(XX), number_strata), dat_splitted, cor_conf, exposure = "ZZ_perc_imm65", .id = "stratum")


  # weighted mean and max
  dat_cor_strata <- dat_cor_strata_all %>%
    mutate(cor_weight = cor * n_counties) %>%
    group_by(confounder) %>%
    mutate(tot_counties = sum(n_counties)) %>%
    group_by(exposure, confounder) %>%
    summarize(
      weighted_mean_strata = sum(abs(cor_weight)) / first(tot_counties),
      crude_mean_strata = mean(abs(cor)),
      max_strata = max(abs(cor)),
      .groups = "drop_last"
    )


  # @@@@@@@@@@@@
  # All togheter
  # @@@@@@@@@@@@
  all_cor <- dat_cor_strata %>%
    pivot_longer(cols = c("weighted_mean_strata", "crude_mean_strata", "max_strata"), names_to = "metric") %>%
    full_join(dat_cor_crude, by = c("exposure", "confounder")) %>%
    rename(no_strata = cor, strata = value) %>%
    pivot_longer(cols = c("no_strata", "strata"), names_to = "type") %>%
    select(-n_counties) %>%
    mutate(value = abs(value)) # this is for the crude


  list(dat_cor_crude = dat_cor_crude, dat_cor_strata_all = dat_cor_strata_all, all_cor = all_cor)
}


#' count_counties
#'
#' Counties per date
#'
#' @param dat 
#' @param date_c 
#' @param filt_c 
#'
#' @return
#' @export
#'
#' @examples
count_counties <- function(dat, date_c, filt_c = 1){
  dat %>% 
    dplyr::filter(date == !!date_c, NC_cases >= !!filt_c) %>% 
    group_by(date) %>% 
    summarise(n = n(), .groups = "drop_last")
  
}
