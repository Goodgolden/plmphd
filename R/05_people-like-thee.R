## 5.1 dynamic-people-like-me --------------------------------------------------

#' People-Like-Me for Dynamic Prediction
#'
#' @description This function predicts outcomes for a testing dataset using a dynamic prediction method. It fits a brokenstick-like model using \code{lme4::lmer()} on the training dataset, which can include time-variant and -invariant variables, as well as interactions. The model is then applied to predict outcomes in the testing dataset at specified anchor times. A matching step using Euclidean or Mahalanobis distances for individuals is included. The final model is a weighted GAMLSS model with user-defined formulas for the mean and sigma functions. The function returns predicted values for the testing dataset, including observation times, predictive times, and predicted values.
#'
#' @details The brokenstick-like model is based on \code{lme4::lmer()} and the dynamic prediction uses the \code{JMbayes2::IndvPred_lme()} method. Individual matching is based on distance measures, and predictions are made using a weighted GAMLSS model for each individual.
#'
#' @param train_data \code{data.frame}. The training dataset.
#' @param test_data \code{data.frame}. The testing dataset.
#' @param new_data \code{data.frame}. A new dataset for prediction.
#' @param outcome_var \code{character}. The name of the outcome variable in the dataset.
#' @param time_var \code{character}. The name of the time variable in the dataset.
#' @param id_var \code{character}. The name of the ID variable in the dataset.
#' @param tmin \code{numeric}. The minimum time point for prediction.
#' @param tmax \code{numeric}. The maximum time point for prediction.
#' @param brokenstick_knots \code{numeric vector}. The knots for the brokenstick model.
#' @param anchor_time \code{numeric vector}. The anchor time point for the prediction.
#' @param gamlss_formula \code{formula or text}. The formula for the GAMLSS model's mean function.
#' @param gamlss_sigma \code{formula or text}. The formula for the GAMLSS model's sigma function.
#' @param match_methods \code{character}. The distance metric used for matching (e.g., \code{"euclidean"} or \code{"mahalanobis"}).
#' @param weight \code{logical}. Whether to include the weights for the GAMLSS model.
#' @param match_alpha \code{numeric}. Alpha level for matching (e.g., \code{0.05}).
#' @param match_number \code{integer}. The number of matches to include for each individual.
#' @param match_plot \code{logical}. If \code{TRUE}, a plot of the matching process will be generated.
#' @param predict_plot \code{logical}. If \code{TRUE}, a plot of the predicted and observed values will be generated.
#' @param ... Additional arguments passed to other methods.
#'
#' @usage people_like_thee(train_data, test_data, new_data, outcome_var, time_var, id_var, ...)
#'
#' @note
#' \describe{
#'   \item{\code{matching_number} and \code{matching_alpha}}{Only one of these can be used for the Mahalanobis distance matching method.}
#'   \item{Euclidean and \code{matching_number}}{Only \code{matching_number} can be used for the Euclidean distance matching method.}
#' }
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{centiles_observed}}{\code{numeric}. The observed centiles based on the testing dataset's observed values.}
#'   \item{\code{centiles_predicted}}{\code{numeric}. The predicted centiles based on the testing dataset's predicted values.}
#'   \item{\code{plot}}{\code{ggplot2 plot}. A plot showing both observed and predicted centiles, if \code{predict_plot = TRUE}.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result <- people_like_thee(
#'   train_data = train_data,
#'   test_data = test_data,
#'   new_data = new_data,
#'   outcome_var = "outcome",
#'   time_var = "time",
#'   id_var = "id",
#'   tmin = 0,
#'   tmax = 10,
#'   brokenstick_knots = c(1, 3, 5),
#'   anchor_time = 5,
#'   gamlss_formula = y ~ x,
#'   gamlss_sigma = ~ z,
#'   match_methods = "mahalanobis",
#'   weight = NULL,
#'   match_alpha = 0.95,
#'   # match_number = 5,  # only use one method
#'   match_plot = TRUE,
#'   predict_plot = TRUE
#' )
#'
#' # Accessing results
#' print(result$centiles_observed)
#' print(result$centiles_predicted)
#' }


people_like_thee <- function(train_data,
                            test_data,
                            new_data,
                            outcome_var = "ht",
                            time_var = "time",
                            id_var = "id",
                            tmin = 0, tmax = 17,
                            bks_fixed = "1 + bs(time, df = 5, degree = 1) * sex",
                            bks_random = "1 + bs(time, df = 5, degree = 1) * sex",
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
  ## I do not know whether there is a better way of doing this
  ## change the vector or list type into a string type
  browser()
  anchor_string <- paste(anchor_time, collapse = ",")

  ## do not need to do the dynamic prediction for the training set
  ## directly use the brokenstick prediction for the training set
  # id_train <- dplyr::select(train_data, !!id_var) %>%
  # unique() %>% unlist()
  id_test <- dplyr::select(test_data,
                  !!id_var) %>%
    unique() %>%
    unlist()

  ctl <- .makeCC("warning", tol = 1e-3)
  cat("\n Fitting the brokenstick model\n")

  ## need to rewrite this part into formula
  ## in the way the user can specify the variables
  ## and the spline function
  ## should let the user define the formula for the brokenstick
  ## for both the random and fixed effects parts
  # bks_fixed <- paste0(1 +bs(time, knots = c(", bks_string, "), degree = 1) * sex")
  # bks_formula_fixed <- paste0(outcome_var, " ~ ", bks_fixed)
  # bks_random <- paste0(1 + bs(time, knots = c(", bks_string, "), degree = 1) * sex")
  # bks_formula_random <- paste0("(", bks_random, " | ", id_var, ")")

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

  ## Here is to check the singularity of the model
  ## if any turns into zero, then the model is singular
  ## it takes a longer time but it will work with `optimx()`
  summary(rePCA(bks_lmer))

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
                 ~IndvPred_lmer(lmerObject = bks_lmer,
                                data = train_data,
                                newdata = .,
                                timeVar = time_var,
                                outcomeVar = outcome_var,
                                idVar = id_var,
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
  ## It is hard to write the design matrix for training
  # anchor_time <- eval(parse(text = anchor_time))
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



