#' Title Original People-Like-Me for Single-time Matching
#'
#' @param train_data The training dataset for brokenstick model and linear model fitting; This training dataset also serves as the pool for matching process.
#' @param test_data The testing dataset for the personalized prediction.
#' @param outcome_var The outcome variable of interest.
#' @param time_var The time variable of interest.
#' @param id_var The id variable of interest.
#' @param tmin The minimum time point for the prediction.
#' @param tmax The maximum time point for the prediction.
#' @param brokenstick_knots The knots for the brokenstick model, which does not need to be equal distanced.
#' @param anchor_time The time point for the anchor time for the matching process.
#' @param linear_formula The formula for the linear model.
#' @param gamlss_formula The mean formula for the GAMLSS model, mainly used as a smoothing process only include the function of time. Here we use the GAMLSS model as a non-parametric / semi-parametric functional process.
#' @param gamlss_sigma The sigma formula for the GAMLSS model, mainly used as a smoothing process only include the function of time. Here we use the GAMLSS model as a non-parametric / semi-parametric functional process.
#' @param match_number The number of matches for the matching process, which can be NULL for no number of matches, or a numeric value for the number of matches.
#' @param match_plot The logical value for the matching plot, which can be TRUE for the plot, FALSE for no plot.
#' @param predict_plot The logical value for the prediction plot, which can be TRUE for the plot, FALSE for no plot.
#' @param ...
#' @return
#' @export
#' @examples

plm_single <- function(train_data,
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



#' Title People-Like-Me methods for single testing individual
#'
#' @param train_data
#' @param test_data
#' @param outcome_var
#' @param time_var
#' @param id_var
#' @param brokenstick_knots
#' @param anchor_time
#' @param linear_formula
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
#'
#' @examples
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


#' Title People-Like-Me methods for multiple testing dataset individuals
#'
#' @param train_data The training dataset for brokenstick model and linear model fitting; This training dataset also serves as the pool for matching process.
#' @param test_data The testing dataset for the personalized prediction.
#' @param outcome_var The outcome variable of interest.
#' @param time_var The time variable of interest.
#' @param id_var The id variable of interest.
#' @param tmin The minimum time point for the prediction.
#' @param tmax The maximum time point for the prediction.
#' @param brokenstick_knots The knots for the brokenstick model, which does not need to be equal distanced.
#' @param anchor_time The time point for the anchor time for the matching process.
#' @param linear_model The type of linear model, which can be "lm", "mlm", "gls".
#' @param linear_formula The formula for the linear model.
#' @param gamlss_formula The mean formula for the GAMLSS model, mainly used as a smoothing process only include the function of time. Here we use the GAMLSS model as a non-parametric / semi-parametric functional process.
#' @param gamlss_sigma The sigma formula for the GAMLSS model, mainly used as a smoothing process only include the function of time. Here we use the GAMLSS model as a non-parametric / semi-parametric functional process.
#' @param match_methods The methods for matching, which can be "euclidean" for multiple time point matching with Euclidean distance, "mahalanobis" for multiple time point mathcing with Mahalanobis distance, "single" for single time point predictive mean matching.
#' @param weight The weight for the final predictive GAMLSS model, which can be FALSE for no weight, TRUE for the weight from the matching process, "p-value" for the weight from the p-value of the Mahalanobis distance \chi-square distribution.
#' @param match_alpha The alpha level for the matching process based only on Mahalnobis distance, which can be NULL for no alpha level, or a numeric value for the alpha level.
#' @param match_number The number of matches for the matching process, which can be NULL for no number of matches, or a numeric value for the number of matches.
#' @param match_plot The logical value for the matching plot, which can be TRUE for the plot, FALSE for no plot.
#' @param predict_plot The logical value for the prediction plot, which can be TRUE for the plot, FALSE for no plot.
#' @param ...

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








