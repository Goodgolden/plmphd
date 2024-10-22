## 3.1 people_like_i --------------------------------------------------------------
#' People-Like-Me for Single-Time Point Matching for Single Individual
#'
#' @description This function, part of the People-Like-Me method, predicts personalized centiles for a testing dataset based on a training dataset. It first fits a brokenstick model and a linear model to the training data, then predicts outcomes for the test data using the linear model. Matches for the test dataset are determined based on single time point predictions. Finally, the function predicts centiles for the test dataset using a GAMLSS model, with optional plots if `predict_plot` is set to \code{TRUE}.
#'
#' @param train_data \code{data.frame} The training dataset for fitting the brokenstick and linear models. This dataset also serves as the pool for the matching process.
#' @param test_data \code{data.frame} The testing dataset for personalized prediction.
#' @param outcome_var \code{character} The name of the outcome variable of interest.
#' @param time_var \code{character} The name of the time variable of interest.
#' @param id_var \code{character} The name of the ID variable representing individuals.
#' @param tmin \code{numeric} The minimum time point for the prediction.
#' @param tmax \code{numeric} The maximum time point for the prediction.
#' @param brokenstick_knots \code{numeric} A vector of knots for the brokenstick model, which do not need to be equally spaced.
#' @param anchor_time \code{numeric} The specific time point used as the anchor for the matching process.
#' @param linear_formula \code{formula} The formula used to fit the linear model.
#' @param gamlss_formula \code{formula} The mean formula for the GAMLSS model, typically used for smoothing as a function of time.
#' @param gamlss_sigma \code{formula} The sigma formula for the GAMLSS model, used for smoothing as a function of time.
#' @param match_number \code{numeric} The number of matches to find during the matching process. Can be \code{NULL} or a numeric value.
#' @param match_plot \code{logical} If \code{TRUE}, a plot of the matching process will be generated. Defaults to \code{TRUE}.
#' @param predict_plot \code{logical} If \code{TRUE}, a plot of the prediction process will be generated. Defaults to \code{TRUE}.
#' @param ... Other arguments passed to the function.
#'
#' @return A \code{list} containing the following attributes:
#' \itemize{
#'   \item \code{predicted_centiles}: A \code{data.frame} of predicted centiles for each individual in the test dataset.
#'   \item \code{observed_centiles}: A \code{data.frame} of observed centiles for each individual in the test dataset.
#'   \item \code{plots}: A \code{ggplot} object of the prediction plot, if \code{predict_plot = TRUE}.
#'   \item \code{subset}: A list of matched individuals based on the single-time point matching process.
#'   \item \code{linear}: The fitted linear model used to predict outcomes.
#'   \item \code{brokenstick_model}: The fitted brokenstick model used for initial data imputation and prediction.
#'   \item \code{matching_plot}: A plot object of the matching process, if \code{match_plot = TRUE}.
#' }
#'
#' @seealso \code{\link{people_like_me}}, \code{\link{people_like_us}}, \code{\link{people_like_thee}}
#'
#' @examples
#' \dontrun{
#'   # Example usage
#'   results <- people_like_i(train_data, test_data, outcome_var = "height", time_var = "age", id_var = "id", ...)
#' }
#'
#' @export


people_like_i <- function(train_data,
                       test_data,
                       outcome_var,
                       time_var,
                       id_var,
                       tmin,
                       tmax,
                       brokenstick_knots,
                       anchor_time,
                       linear_formula,
                       gamlss_formula,
                       gamlss_sigma,
                       match_number,
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

  ## extract the test baseline information
  test_baseline <- test_data %>%
    group_by(!!id_var) %>%
    arrange({{time_var}}) %>%
    slice(1L) %>%
    ## change the baseline outcome_vars as new variable
    # dplyr::select(baseline = !!outcome_var, everything()) %>%
    ## move the original time_var as all ZEROs
    dplyr::select(-!!time_var)

  ## brokenstick model  ------------------------------
  ## will add other methods probably will add ifelse
  ## currently just the brokenstick model
  brokenstick <- impute_brokenstick(outcome_var = !!outcome_var,
                                    time_var = !!time_var,
                                    id_var = !!id_var,
                                    bs_knots = brokenstick_knots,
                                    anchor_time = anchor_time,
                                    data = train_data)

  ## linear model section ---------------------
  if (is.null(linear_formula)) {
    stop("Please specify the type of linear model")}
  ### single linear model with time as independent factor ----------------------

  lm_bks <- lm(as.formula(linear_formula),
               data = brokenstick)
  # test_baseline[paste0("anchor_", anchor_time)] <- NA

  data_test1 <- test_baseline %>%
    # dplyr::select(-!!time_var) %>%
    ## this is the unnest way of doing testing dataset
    mutate(time = list(anchor_time)) %>%
    unnest(cols = time) %>%
    rename(baseline = !!outcome_var)

  lp_test <- data_test1 %>%
    ungroup() %>%
    mutate(lm_bks_target = predict(lm_bks, newdata = .)) %>%
    dplyr::select(!!id_var, !!time_var, contains("lm_bks_target")) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rename(!!outcome_var := lm_bks_target)

  lp_train <- brokenstick %>%
    ungroup() %>%
    mutate(lm_bks_target = predict(lm_bks)) %>%
    dplyr::select(!!id_var, !!time_var, contains("lm_bks_target")) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rename(!!outcome_var := lm_bks_target)

  ## need to be changed if the other methods need to be used
  cat("\n Finding Matches with Single Time Prediction\n")

  ## distance and matches finding ----------------------------------
  ## based on single time point is easier
  subset <- lp_test %>%
    group_by(!!id_var) %>%
    group_map(~ dis_match(lb_train = lp_train,
                          lb_test_ind = .,
                          train = train_data,
                          match_methods = "single",
                          id_var = !!id_var,
                          outcome_var = !!outcome_var,
                          time_var = !!time_var,
                          match_number = match_number,
                          match_time = anchor_time,
                          match_plot = match_plot)%>%
                suppressMessages(),
              .progress = TRUE)

  cat("\n Final Prediction is almost done! \n")
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
                             # weight = weight,
                             gamlss_formula = gamlss_formula,
                             gamsigma_formula = gamlss_sigma,
                             predict_plot = predict_plot) %>%
                suppressMessages()),
         .progress = TRUE)

  ## attributes ready ---------------------------------
  attr(results, "subset") <- subset
  attr(results, "linear") <- lm_bks
  # attr(results, "matching_plot") <- subset$matching_plot
  attr(results, "brokenstick_model") <- brokenstick
  # attr(results, "brokenstick_impute") <- brokenstick$data_anchor
  # attr(results, "baseline") <- brokenstick$data_baseline
  # attr(results, "linear_model") <- summary(lm_bks)

  return(results)
}


## 3.2 people-like-me ---------------------------------------------------------
#' People-Like-Me Methods for Single Testing Individual
#'
#' @description This function is part of the People-Like-Me method and is designed for personalized prediction of centiles for a single individual in the testing dataset. The function fits a brokenstick and linear model to the training data, and then uses the models to make predictions for the testing individual. Matching is performed based on multiple anchor time points using various distance metrics such as Euclidean and Mahalanobis distance. The function returns predicted centiles using the GAMLSS model, and includes the option to visualize the matching and prediction process with plots.
#'
#' @param train_data \code{data.frame} The training dataset for fitting the brokenstick and linear models. This dataset also serves as the pool for the matching process.
#' @param test_data \code{data.frame} The testing dataset for personalized prediction.
#' @param outcome_var \code{character} The name of the outcome variable of interest.
#' @param time_var \code{character} The name of the time variable of interest.
#' @param id_var \code{character} The name of the ID variable representing individuals.
#' @param tmin \code{numeric} The minimum time point for the prediction.
#' @param tmax \code{numeric} The maximum time point for the prediction.
#' @param brokenstick_knots \code{numeric} A vector of knots for the brokenstick model, which do not need to be equally spaced.
#' @param anchor_time \code{numeric} A time point (or multiple time points) used for the anchor in the matching process.
#' @param linear_formula \code{formula} The formula used to fit the linear model.
#' @param gamlss_formula \code{formula} The mean formula for the GAMLSS model, typically used for smoothing as a function of time.
#' @param gamlss_sigma \code{formula} The sigma formula for the GAMLSS model, used for smoothing as a function of time.
#' @param match_methods \code{character} Distance methods used for matching. Options are \code{"euclidean"}, \code{"mahalanobis"}, or \code{"single"}.
#' @param weight \code{logical} Whether to apply weights in the GAMLSS prediction process. Defaults to \code{FALSE}.
#' @param match_alpha \code{numeric} Alpha parameter for the matching process, typically used in Mahalanobis distance. Can be \code{NULL}.
#' @param match_number \code{numeric} The number of matches to find during the matching process. Can be \code{NULL} or a numeric value.
#' @param match_plot \code{logical} If \code{TRUE}, a plot of the matching process will be generated. Defaults to \code{TRUE}.
#' @param predict_plot \code{logical} If \code{TRUE}, a plot of the prediction process will be generated. Defaults to \code{TRUE}.
#' @param ... Other arguments passed to the function.
#'
#' @return A \code{list} containing the following elements:
#' \itemize{
#'   \item \code{predicted_centiles}: A \code{data.frame} of predicted centiles for the individual in the test dataset.
#'   \item \code{observed_centiles}: A \code{data.frame} of observed centiles for the individual in the test dataset.
#'   \item \code{matches}: A plot of the matching process, if \code{match_plot = TRUE}.
#'   \item \code{plot}: A plot of the predicted centiles, if \code{predict_plot = TRUE}.
#'   \item \code{gamlss_data}: A \code{data.frame} of the subset used for the GAMLSS prediction.
#' }
#'
#' @seealso \code{\link{people_like_i}}, \code{\link{people_like_us}}, \code{\link{people_like_thee}}
#'
#' @examples
#' \dontrun{
#'   # Example usage
#'   results <- people_like_me(train_data, test_data, outcome_var = "height", time_var = "age", id_var = "id", ...)
#' }
#'
#' @export

people_like_me <- function(train_data,
                           test_data,
                           outcome_var,
                           time_var,
                           id_var,
                           tmin,
                           tmax,
                           brokenstick_knots,
                           anchor_time,
                           linear_formula,
                           gamlss_formula,
                           gamlss_sigma,
                           match_methods = c("euclidean", "mahalanobis", "single"),
                           weight = FALSE,
                           match_alpha = NULL,
                           match_number = NULL,
                           match_plot = TRUE,
                           predict_plot = TRUE,
                           ...) {

  ## user defined variables
  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)

  ## extract the test baseline information
  test_baseline <- test_data %>%
    group_by(!!id_var) %>%
    arrange(!!time_var) %>%
    slice(1L) %>%
    ## change the baseline outcome_vars as new variable
    # dplyr::select(baseline = !!outcome_var, everything()) %>%
    ## move the original time_var as all ZEROs
    dplyr::select(- !!time_var)

  # Tue Jul 25 22:09:42 2023 ------------------------------
  ## will add other methods probably will add ifelse
  ## currently just the brokenstick model
  brokenstick <- impute_brokenstick(outcome_var = !!outcome_var,
                                    time_var = !!time_var,
                                    id_var = !!id_var,
                                    bs_knots = brokenstick_knots,
                                    anchor_time = anchor_time,
                                    data = train_data)

  ## linear regression is the necessary one will be kept
  lm_bks <- lm(as.formula(linear_formula),
               data = brokenstick)
  # Tue Jul 25 22:30:34 2023 ------------------------------
  test_baseline[paste0("anchor_", anchor_time)] <- NA

  data_test1 <- test_baseline %>%
    # dplyr::select(-!!time_var) %>%
    group_by(!!id_var) %>%
    pivot_longer(cols = contains("anchor_"),
                 names_to = "time0",
                 names_prefix = "anchor_",
                 values_to = "lm_bks_target") %>%
    rename(baseline = !!outcome_var,
           !!time_var := time0)

  lp_test <- data_test1 %>%
    ungroup() %>%
    mutate(lm_bks_target = predict(lm_bks, newdata = data_test1)) %>%
    dplyr::select(!!id_var, !!time_var, contains("lm_bks_target")) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rename(!!outcome_var := lm_bks_target)

  lp_train <- brokenstick %>%
    ungroup() %>%
    mutate(lm_bks_target = predict(lm_bks)) %>%
    dplyr::select(!!id_var, !!time_var, contains("lm_bks_target")) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rename(!!outcome_var := lm_bks_target)

  ## end of 01_impute.R file ------------------------

  ## this is the distance for just one individual
  distance <- distance_df(lb_train = lp_train,
                          lb_test_ind = lp_test,
                          match_methods = match_methods,
                          id_var = !!id_var,
                          outcome_var = !!outcome_var,
                          time_var = !!time_var)

  subset <- match(distance_df = distance,
                  train = train_data,
                  test_one = test_data,
                  id_var = !!id_var,
                  outcome_var = !!outcome_var,
                  time_var = !!time_var,
                  match_alpha = match_alpha,
                  match_number = match_number,
                  match_plot = match_plot)

  ## the dataset is ready ---------------------------
  gamlss1 <- predict_gamlss(matching = subset$subset,
                            test_one = test_data,
                            id_var = !!id_var,
                            time_var = !!time_var,
                            outcome_var = !!outcome_var,
                            weight = weight,
                            tmin = tmin,
                            tmax = tmax,
                            gamlss_formula = gamlss_formula,
                            gamsigma_formula = gamlss_sigma,
                            predict_plot = predict_plot)

  results <- list(plot = gamlss1$predictive_centiles,
                  matches = subset$plot,
                  predicted = gamlss1$centiles_predicted,
                  observed = gamlss1$centiles_observed,
                  gamlss_data = subset$subset)

  attr(results, "distance") <- distance
  attr(results, "brokenstick_model") <- brokenstick$model_bks
  attr(results, "brokenstick_impute") <- brokenstick$data_anchor
  attr(results, "linear_model") <- lm_bks

  return(results)
}

## 3.3 people-like-us ----------------------------------------------------------
#' People-Like-Me Methods for Multiple Testing Individuals
#'
#' @description This function is part of the People-Like-Me method and is designed for personalized prediction of centiles for multiple individuals in the testing dataset. The function fits a brokenstick and linear model to the training data, and then uses the models to make predictions for multiple testing individuals. Matching is performed based on various distance metrics such as Euclidean or Mahalanobis distance or by single time point matching. The function returns predicted centiles using the GAMLSS model, with options to visualize both the matching and prediction processes.
#'
#' @param train_data \code{data.frame} The training dataset for fitting the brokenstick and linear models. This dataset also serves as the pool for the matching process.
#' @param test_data \code{data.frame} The testing dataset for personalized prediction.
#' @param outcome_var \code{character} The name of the outcome variable of interest.
#' @param time_var \code{character} The name of the time variable of interest.
#' @param id_var \code{character} The name of the ID variable representing individuals.
#' @param tmin \code{numeric} The minimum time point for the prediction.
#' @param tmax \code{numeric} The maximum time point for the prediction.
#' @param brokenstick_knots \code{numeric} A vector of knots for the brokenstick model, which do not need to be equally spaced.
#' @param anchor_time \code{numeric} A time point (or multiple time points) used for the anchor in the matching process.
#' @param linear_model \code{character} The type of linear model to be used. Options are \code{"lm"} for a single linear model, \code{"mlm"} for a multiple linear model, and \code{"gls"} for generalized least squares.
#' @param linear_formula \code{formula} The formula used to fit the linear model.
#' @param gamlss_formula \code{formula} The mean formula for the GAMLSS model, typically used for smoothing as a function of time.
#' @param gamlss_sigma \code{formula} The sigma formula for the GAMLSS model, used for smoothing as a function of time.
#' @param match_methods \code{character} Distance methods used for matching. Options are \code{"euclidean"}, \code{"mahalanobis"}, or \code{"single"}.
#' @param weight \code{logical} Whether to apply weights in the GAMLSS prediction process. Defaults to \code{FALSE}.
#' @param match_alpha \code{numeric} Alpha parameter for the matching process, typically used in Mahalanobis distance. Can be \code{NULL}.
#' @param match_number \code{numeric} The number of matches to find during the matching process. Can be \code{NULL} or a numeric value.
#' @param match_plot \code{logical} If \code{TRUE}, a plot of the matching process will be generated. Defaults to \code{TRUE}.
#' @param predict_plot \code{logical} If \code{TRUE}, a plot of the prediction process will be generated. Defaults to \code{TRUE}.
#' @param ... Other arguments passed to the function.
#'
#' @return A \code{list} containing the following elements:
#' \itemize{
#'   \item \code{predicted_centiles}: A \code{data.frame} of predicted centiles for individuals in the test dataset.
#'   \item \code{observed_centiles}: A \code{data.frame} of observed centiles for individuals in the test dataset.
#'   \item \code{matches}: A plot of the matching process, if \code{match_plot = TRUE}.
#'   \item \code{plot}: A plot of the predicted centiles, if \code{predict_plot = TRUE}.
#'   \item \code{gamlss_data}: A \code{data.frame} of the subset used for the GAMLSS prediction.
#' }
#'
#' @seealso \code{\link{people_like_me}}, \code{\link{people_like_i}}, \code{\link{people_like_thee}}
#'
#' @examples
#' \dontrun{
#'   # Example usage with the internal dataset
#'   results <- people_like_us(train_data, test_data, outcome_var = "height", time_var = "age", id_var = "id", ...)
#' }
#'
#' @export

people_like_us <- function(train_data,
                           test_data,
                           outcome_var = "ht",
                           time_var = "time",
                           id_var = "id",
                           tmin,
                           tmax,
                           brokenstick_knots,
                           anchor_time,
                           linear_model = c("lm", "mlm", "gls"),
                           linear_formula,
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

  ## extract the test baseline information
  test_baseline <- test_data %>%
    group_by(!!id_var) %>%
    arrange({{time_var}}) %>%
    slice(1L) %>%
    ## change the baseline outcome_vars as new variable
    # dplyr::select(baseline = !!outcome_var, everything()) %>%
    ## move the original time_var as all ZEROs
    dplyr::select(-!!time_var)

  ## brokenstick model  ------------------------------
  ## will add other methods probably will add ifelse
  ## currently just the brokenstick model
  brokenstick <- impute_brokenstick(outcome_var = !!outcome_var,
                                    time_var = !!time_var,
                                    id_var = !!id_var,
                                    bs_knots = brokenstick_knots,
                                    anchor_time = anchor_time,
                                    data = train_data)

  ## linear model section ---------------------
  if (is.null(linear_model)) {
    stop("Please specify the type of linear model")}
  ### single linear model with time as independent factor ----------------------
  if (linear_model == "lm") {
    lm_bks <- lm(as.formula(linear_formula),
                 data = brokenstick)
    # test_baseline[paste0("anchor_", anchor_time)] <- NA

    data_test1 <- test_baseline %>%
      # dplyr::select(-!!time_var) %>%
      ## this is the unnest way of doing testing dataset
      mutate(time = list(anchor_time)) %>%
      unnest(cols = time) %>%
      rename(baseline = !!outcome_var)

    lp_test <- data_test1 %>%
      ungroup() %>%
      mutate(lm_bks_target = predict(lm_bks, newdata = .)) %>%
      dplyr::select(!!id_var, !!time_var, contains("lm_bks_target")) %>%
      as.matrix() %>%
      as.data.frame() %>%
      rename(!!outcome_var := lm_bks_target)

    lp_train <- brokenstick %>%
      ungroup() %>%
      mutate(lm_bks_target = predict(lm_bks)) %>%
      dplyr::select(!!id_var, !!time_var, contains("lm_bks_target")) %>%
      as.matrix() %>%
      as.data.frame() %>%
      rename(!!outcome_var := lm_bks_target)}

  ### multiple linear model ---------------------------------------
  if (linear_model == "mlm") {
    data_test1 <- test_baseline %>%
      # dplyr::select(-!!time_var) %>%
      ## this is the unnest way of doing testing dataset
      # mutate(time = list(anchor_time)) %>%
      # unnest(cols = !!time_var) %>%
      ## original way of augment the testing dataset
      # group_by(!!id_var) %>%
      # pivot_longer(cols = contains("anchor_"),
      #              names_to = "time0",
      #              names_prefix = "anchor_",
      #              values_to = "lm_bks_target") %>%
      rename(baseline = !!outcome_var)

    lm_bks <- brokenstick %>%
      group_by(!!time_var) %>%
      group_split() %>%
      map(~lm(as.formula(linear_formula), .x))

    lp_test <- map_dfr(lm_bks,
                    ~ data_test1 %>%
                         ungroup() %>%
                         mutate(lm_bks_target = predict(.x, newdata = .)) %>%
                         dplyr::select(!!id_var, contains("lm_bks_target")) %>%
                         as.matrix() %>%
                         as.data.frame() %>%
                         rename(!!outcome_var := lm_bks_target)) %>%
      mutate(time = rep(anchor_time, each = nrow(.)/length(anchor_time))) %>%
      rename(!!time_var := time) %>%
      dplyr::select(!!id_var, !!time_var, !!outcome_var)

    lp_train <- brokenstick %>%
      dplyr::select(!!id_var, !!time_var) %>%
      mutate(lm_bks_target =c(t(map_dfc(lm_bks, predict)))) %>%
        rename(!!outcome_var := lm_bks_target) %>%
      dplyr::select(!!id_var, !!time_var, !!outcome_var)
    }

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
    group_map(~ dis_match(lb_train = lp_train,
                          lb_test_ind = .,
                          train = train_data,
                          match_methods = match_methods,
                          id_var = !!id_var,
                          outcome_var = !!outcome_var,
                          time_var = !!time_var,
                          match_alpha = match_alpha,
                          match_number = match_number,
                          match_time = match_time,
                          match_plot = match_plot),
              .progress = TRUE)

  cat("\n Final Prediction is almost done! \n")
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
  attr(results, "linear") <- lm_bks
  # attr(results, "matching_plot") <- subset$matching_plot
  attr(results, "brokenstick_model") <- brokenstick
  # attr(results, "brokenstick_impute") <- brokenstick$data_anchor
  # attr(results, "baseline") <- brokenstick$data_baseline
  # attr(results, "linear_model") <- summary(lm_bks)

  return(results)
}








