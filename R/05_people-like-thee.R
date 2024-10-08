## 5.1 dynamic-people-like-me --------------------------------------------------
#' Title People-Like-Me for Dynamic Prediction
#'
#' @param train_data
#' @param test_data
#' @param new_data
#' @param outcome_var
#' @param time_var
#' @param id_var
#' @param tmin
#' @param tmax
#' @param brokenstick_knots
#' @param anchor_time
#' @param gamlss_formula
#' @param gamlss_sigma
#' @param match_methods
#' @param weight
#' @param match_alpha
#' @param match_number
#' @param match_plot
#' @param predict_plot
#' @param ...
#'
#' @return
#' @export
people_like_thee <- function(train_data,
                            test_data,
                            new_data,
                            outcome_var = "ht",
                            time_var = "time",
                            id_var = "id",
                            tmin, tmax,
                            brokenstick_knots,
                            anchor_time = "(5, 10, 15)",
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
  ## I do not know whether there is a better way of doing this
  anchor_time <- ensym(anchor_time)
  # id_train <- dplyr::select(train_data, !!id_var) %>%
  # unique() %>% unlist()
  id_test <- dplyr::select(test_data, !!id_var) %>%
    unique() %>%
    unlist()

  ctl <- .makeCC("warning", tol = 1e-3)
  cat("\n Fitting the brokenstick model\n")
  ## need to rewrite this part into formula
  ## in the way the user can specify the variables
  ## and the spline function

  bks_fixed <- "1 + bs(time, knots = c(5, 10, 15), degree = 1) * sex"
  bks_formula_fixed <- paste0(outcome_var, " ~ ", bks_fixed)
  bks_random <- "1 + bs(time, knots = c(5, 10, 15), degree = 1"
  bks_formula_random <- paste0("(", bks_random, " | ", id_var, ")")

  bks_formula <- paste0(bks_formula_fixed, "+", bks_formula_random)
  # paste0(outcome_var, " ~ 1 + bs(", time_var, ", knots = ", anchor_time, ", degree = 1) + (1 + bs(", time_var, ", knots = ", anchor_time, ", degree = 1) | ", id_var, ")")
  bks_nlme <- lmer(bks_formula,
                   na.action = na.exclude,
                   REML = TRUE,
                   control = lmerControl(check.nobs.vs.nRE = "warning",
                                         optCtrl = list(method = 'nlminb'),
                                         optimizer = "optimx"),
                   data = train_data)
  ## Here is to check the singularity of the model
  ## if any turns into zero, then the model is singular
  # summary(rePCA(bks_nlme))

  cat("\n Dynamic Predicting with the brokenstick model\n")
  ## extract the test baseline information -------------------------------------
  # test_baseline <- test_data %>%
  #   group_by(!!id_var) %>%
  #   filter(time <= 5)

  # lp_train <- IndvPred_lmer(lmerObject = bks_nlme,
  #                           data = train_data,
  #                           newdata = train_baseline,
  #                           timeVar = time_var,
  #                           M = 500,
  #                           times = anchor_time,
  #                           all_times = TRUE,
  #                           return_data = TRUE,
  #                           level = 0.5,
  #                           interval = "prediction",
  #                           seed = 555) %>%
  #   na.omit() %>%
  #   dplyr::select(!!id_var, !!time_var, pred) %>%
  #   as.matrix() %>%
  #   as.data.frame()

  test_baseline <- new_data %>%
    group_by(!!id_var) %>%
    group_split()

  ## try just one individual and see the results
  # IndvPred_lmer(lmerObject = bks_nlme,
  #               data = train_data,
  #               newdata = test_baseline[[1]],
  #               timeVar = time_var,
  #               M = 500,
  #               times = anchor_time,
  #               all_times = TRUE,
  #               return_data = FALSE,
  #               level = 0.5,
  #               interval = "prediction",
  #               seed = 555)

  ## map the function to all individuals
  lp_test <- map_dfc(test_baseline,
                 ~IndvPred_lmer(lmerObject = bks_nlme,
                                data = train_data,
                                newdata = .,
                                timeVar = time_var,
                                M = 500,
                                times = anchor_time,
                                all_times = TRUE,
                                return_data = FALSE,
                                level = 0.5,
                                interval = "prediction",
                                seed = 555)) %>%
    suppressMessages()

  ## change the column names, optional step
  ## in case we need to see the error acculmulation
  ## through each step in the algorithm
  names(lp_test) <- unique(id_test)

  ## training outcome_vars -----------------------------------------------------
  ## the original idea is to use the IndvPred_lmer
  ## function to predict the for the training
  ## but why, if we can simple use the brokenstick imputes
  ## 1. save time and resources
  ## 2. brokenstick model is good enough
  # dplyr::select(baseline = !!outcome_var, everything()) %>%
  ## move the original time_var as all ZEROs
  # dplyr::select(-!!time_var)
  # train_baseline <- train_data %>%
  #   group_by(!!id_var) %>%
  #   slice(1L)
  # dplyr::select(-time) %>%
  # dplyr::select(baseline = log_outcome,
  #              everything())

  ## the train matching is labeled as `lb` for training
  ## to indicate it is from brokenstick model
  ## other than the prediction
  ## from the dynamic prediction `lp` for testing
  anchor_time <- eval(parse(text = anchor_time))
  lb_train <- expand.grid(time = anchor_time,
                          id = unique(train_data$id))

  lb_train$pred <- predict(bks_nlme,
                          newdata = lb_train,
                          type = "response")

  lb_train <- lb_train %>%
    pivot_wider(names_from = {{ id_var }},
                values_from = pred) %>%
    column_to_rownames(var = as.character({{ time_var }})) %>%
    mutate_all(as.numeric)

  ## end of 01_impute.R file ------------------------

  if (match_methods == "euclidean") {
    cat("\n Finding Matches with Euclidean distance\n")}
  if (match_methods == "mahalanobis") {
    cat("\n Finding Matches with Mahalanobis distance\n")}
  if (match_methods == "single") {
    cat("\n Finding Matches with Single Time Prediction\n")}
  # save(lp_test, lb_train, file = "data-raw/lp_lb_for_plm_dyn.RData")

  ## distance and matches finding -----------------------------
  subset <- lp_test %>%
    map(~ dyn_match(lb_train = lb_train,
                    lb_test_ind = .,
                    train = train_data,
                    match_methods = match_methods,
                    id_var = !!id_var,
                    outcome_var = "pred",
                    time_var = !!time_var,
                    match_alpha = match_alpha,
                    match_number = match_number,
                    match_time = match_time,
                    ## so far the matching plot is not available
                    ## need to think about this.
                    match_plot = match_plot),
        .progress = TRUE)

  # if (length(test_data) > length(subset)) {
  #   warning("There are", length(test_data) - length(subset), "test baseline contain no dataset before this given time")
  # } else {
  #   plm_plot <- NULL
  # }

  cat("\n Final Prediction is almost done! \n")
  ## final gamlss is ready ---------------------------
  results <- test_data %>%
    group_by(!!id_var) %>%
    group_split() %>%
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


## 5.2 weighted dynamic-people-like-me -----------------------------------------



