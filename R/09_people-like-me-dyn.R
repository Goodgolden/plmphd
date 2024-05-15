

people_like_us_dyn <- function(train_data,
                               test_data,
                               outcome_var = "ht",
                               time_var = "time",
                               id_var = "id",
                               tmin,
                               tmax,
                               brokenstick_knots,
                               anchor_time,
                               gamlss_formula,
                               gamlss_sigma,
                               match_methods = c("euclidean", "mahalanobis", "single"),
                               weight = FALSE,
                               match_alpha = NULL,
                               match_number = NULL,
                               match_plot = TRUE,
                               predict_plot = TRUE,
                               ...) {
  ## user defined variables ----------------------
  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)
  
  # id_train <- dplyr::select(train_data, !!id_var) %>% 
  # unique() %>% unlist()
  id_test <- dplyr::select(test_data, !!id_var) %>%
    unique() %>%
    unlist()
  
  
  # Wed Feb 28 22:32:21 2024 ------------------------------
  ## need to change the option for the optim
  ctl <- lmeControl(opt = "optim")
  # bks_nlme <- lme(log_outcome ~ 1 + bs(time, knots = c(14, 30, 60, 90), degree = 1) +
  #                   surgery_type + patient_gender,
  #                 random = ~1 + bs(time, knots = c(14, 30, 60, 90), degree = 1)|id,
  #                 na.action = na.exclude,
  #                 control = ctl,
  #                 data = tsa_train)
  
  load("newdata/19_dynamic_prediction_bks_lmer_nlme_2024-02-28.RData")
  
  ## extract the test baseline information
  test_baseline <- test_data %>%
    group_by(!!id_var) %>%
    arrange(!!time_var) %>%
    slice(1L) 
  ## change the baseline outcome_vars as new variable
  # dplyr::select(baseline = !!outcome_var, everything()) %>%
  ## move the original time_var as all ZEROs
  # dplyr::select(-!!time_var)
  
  train_baseline <- tsa_train %>%
    group_by(!!id_var) %>%
    arrange(!!time_var) %>%
    slice(1L) 
  
  # dplyr::select(-time) %>%
  # dplyr::select(baseline = log_outcome,
  #              everything())
  # View(train_baseline)
  
  lp_train <- IndvPred_lme(lmeObject = bks_nlme,
                           newdata = train_baseline,
                           timeVar = time_var,
                           M = 500,
                           times = anchor_time,
                           all_times = TRUE,
                           return_data = TRUE,
                           level = 0.5,
                           interval = "prediction",
                           seed = 555) %>%
    na.omit() %>%
    dplyr::select(!!id_var, !!time_var, pred) %>%
    as.matrix() %>%
    as.data.frame()
  
  lp_test <- IndvPred_lme(lmeObject = bks_nlme,
                          newdata = test_baseline,
                          timeVar = time_var,
                          M = 500,
                          times = anchor_time,
                          all_times = TRUE,
                          return_data = TRUE,
                          level = 0.5,
                          interval = "prediction",
                          seed = 555) %>%
    na.omit() %>%
    dplyr::select(!!id_var, !!time_var, pred) %>%
    as.matrix() %>%
    as.data.frame() 
  ## end of 01_impute.R file ------------------------
  
  if (match_methods == "euclidean") {
    cat("\n Finding Matches with Euclidean distance\n")}
  
  if (match_methods == "mahalanobis") {
    cat("\n Finding Matches with Mahalanobis distance\n")}
  
  if (match_methods == "single") {
    cat("\n Finding Matches with Single Time Prediction\n")}
  
  ## distance and matches finding ---------------------------------- 
  subset <- lp_test %>%
    group_by(!!id_var) %>%
    group_map(~ dyn_match(lb_train = lp_train,
                          lb_test_ind = .,
                          train = train_data,
                          match_methods = match_methods,
                          id_var = !!id_var,
                          outcome_var = "pred",
                          time_var = !!time_var,
                          match_alpha = match_alpha,
                          match_number = match_number,
                          match_time = match_time,
                          match_plot = match_plot),
              .progress = TRUE)
  
  ## final gamlss is ready ---------------------------
  results <- test_data %>%
    group_by(!!id_var) %>%
    group_map(~ as.data.frame(.)) %>%
    map2(subset, 
         ~try(predict_gamlss(matching = .y$subset,
                             test_one = .x,
                             id_var = !!id_var,
                             time_var = !!time_var,
                             outcome_var = !!outcome_var,
                             tmin = tmin,
                             tmax = tmax,
                             weight = weight,
                             gamlss_formula = gamlss_formula,
                             gamsigma_formula = gamlss_sigma,
                             predict_plot = predict_plot) %>% 
                suppressMessages()),
         .progress = TRUE)
  
  ## attributes ready ---------------------------------
  attr(results, "subset") <- subset
  # attr(results, "matching_plot") <- subset$matching_plot
  attr(results, "brokenstick_model") <- brokenstick
  # attr(results, "brokenstick_impute") <- brokenstick$data_anchor
  # attr(results, "baseline") <- brokenstick$data_baseline
  # attr(results, "linear_model") <- summary(lm_bks)
  
  return(results)
}






