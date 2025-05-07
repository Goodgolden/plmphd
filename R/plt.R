## 5.1 dynamic-people-like-me --------------------------------------------------
people_like_thee2 <- function(train_data,
                             test_data,
                             new_data,
                             outcome_var = "ht",
                             time_var = "time",
                             id_var = "id",
                             tmin = 0, tmax = 17,
                             bks_fixed = "1 + bs(time, df = 5, degree = 1) * sex",
                             bks_random = "1 + bs(time, df = 5, degree = 1)",
                             anchor_time = c(5, 10, 15),
                             gamlss_formula,
                             gamlss_sigma,
                             match_methods = c("euclidean",
                                               "mahalanobis"),
                             weight = FALSE,
                             match_alpha = NULL,
                             match_number = NULL,
                             match_plot = FALSE,
                             predict_plot = TRUE,
                             ...) {
  ## user defined variables ----------------------
  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)
  bks_fixed <- ensym(bks_fixed)
  bks_random <- ensym(bks_random)
  anchor_string <- paste(anchor_time, collapse = ",")
  id_test <- dplyr::select(test_data,
                           !!id_var) %>%
    unique() %>% unlist()

  ctl <- .makeCC("warning", tol = 1e-2)
  cat("\n Fitting the brokenstick model\n")

  bks_form <- paste0(outcome_var, "~", bks_fixed,
                     " + ", "(", bks_random,
                     " | ", id_var, ")")
  bks_lmer <- lmer(bks_form,
                   na.action = na.exclude,
                   REML = TRUE,
                   control = lmerControl(check.nobs.vs.nRE = "warning",
                                         optCtrl = list(method = 'nlminb'),
                                         optimizer = "optimx"),
                   data = train_data)

  cat("\n Dynamic Predicting with the brokenstick model\n")

  test_baseline <- new_data %>%
    group_by(!!id_var) %>%
    group_split()
  lp_test <- map_dfr(test_baseline,
                     ~IndvPred_lmer(lmerObject = bks_lmer,
                                    data = train_data,
                                    newdata = .,
                                    lmer.fixed = !!bks_fixed,
                                    lmer.random = !!bks_random,
                                    timeVar = !!time_var,
                                    outcomeVar = !!outcome_var,
                                    idVar = !!id_var,
                                    M = 500,
                                    times = anchor_time,
                                    all_times = TRUE,
                                    return_data = FALSE,
                                    level = 0.5,
                                    interval = "prediction",
                                    seed = 555) %>%
                       as.data.frame()) %>%
    suppressMessages()

  cat("\n lp_test done\n")
  lb_train <- expand.grid(time = anchor_time,
                          id = unique(train_data$id)) %>%
    left_join(train_data %>% dplyr::select(id, sex), by = c("id")) %>%
    unique()

  lb_train$pred <- predict(bks_lmer,
                           newdata = lb_train,
                           type = "response")

  ## does not work ------------------------------------------------
  lb_train <- lb_train %>%
    dplyr::select(-sex) %>%
    pivot_wider(names_from = {{ id_var }},
                values_from = pred) %>%
    column_to_rownames(var = as.character({{ time_var }})) %>%
    mutate_all(as.numeric)
  if (match_methods == "euclidean") {
    cat("\n Finding Matches with Euclidean distance\n")}
  if (match_methods == "mahalanobis") {
    cat("\n Finding Matches with Mahalanobis distance\n")}
  if (match_methods == "single") {
    cat("\n Finding Matches with Single Time Prediction\n")}
  subset <- lp_test %>%
    group_by(!!id_var) %>%
    group_map(~ dyn_match(lb_train = lb_train,
                          lb_test_ind = .,
                          train = train_data,
                          match_methods = match_methods,
                          id_var = !!id_var,
                          outcome_var = "pred",
                          time_var = !!time_var,
                          match_alpha = match_alpha,
                          match_number = match_number,
                          match_time = match_time,
                          match_plot = FALSE),
              .progress = TRUE)
  cat("\n Final Prediction is almost done! \n")
  ## final gamlss is ready ---------------------------
  results <- test_data %>%
    group_by(!!id_var) %>%
    group_split() %>%
    map2(subset,
         ~try(predict_gamlss(matching = .y$subset,
                             test_one = .x %>% filter(!!time_var > tmin),
                             id_var = !!id_var,
                             time_var = !!time_var,
                             outcome_var = !!outcome_var,
                             tmin = tmin,
                             tmax = tmax,
                             weight = weight,
                             gamlss_formula = gamlss_formula,
                             gamsigma_formula = gamlss_sigma,
                             predict_plot = predict_plot) %>%
                suppressWarnings()),
         .progress = TRUE)

  return(results)
}

