# First level imputation -------------------------------------------------------

## 1.1 impute_brokenstick -----------------------------------------------------
#' Title First Stage model imputation with `brokenstick::brokenstick()` at anchor time points
#' @description
#' The predictions from a broken stick model coincide with
#' the group-conditional means of the random effects.
#' This function takes the `data` with selected `outcome_var`, `time_var`,
#' and `id_var`. The user can calculate prediction (imputation) at
#' given anchor `time` set.
#'
#' @param outcome_var The outcome_var variable name must include in the dataset
#' @param time_var The time variable name must be included in the dataset
#' @param id_var The id variable name must be included in the dataset
#' @param bs_knots The internal knots for brokenstick model
#' @param anchor_time The anchor time set for imputation,
#' @param data A data frame in which to look for variables with which to fit
#' the brokenstick model and predict. Ideally, this is a longitudinal
#' data.frame object in long-format.
#' @param ... Not used, but required for future extension
#'
#' @return A data frame with the imputed values
#' @export
#' @examples \dontrun {}
## need to add the example and see how things going
impute_brokenstick <- function(outcome_var,
                               time_var,
                               id_var,
                               bs_knots,
                               anchor_time,
                               data,
                               ...) {
  ## use non-standard evaluation for the variables
  ## use ensym() function to passing a string
  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)

  ## the customized formula for the broken-stick model
  formula <- paste0(outcome_var, "~", time_var, "|", id_var)

  ## fit the broken-stick model
  # Tue Jul 11 10:17:57 2023 ------------------------------
  ## probably need to rewrite the brokenstick function
  ## with easier function from brokenstick
  model_bks <- brokenstick::brokenstick(formula = as.formula(formula),
                                        data = data,
                                        knots = bs_knots)


  ## prediction from broke-stick model
  data_anchor <- predict(model_bks,
                         ## predict at the arg pred_time
                         include_data = FALSE,
                         x = anchor_time) %>%
    ## remove the variables without values
    select_if(not_all_na)

  ## pull out the baseline information
  ## need the baseline outcome_var and demo for further steps
  data_baseline <- data %>%
    group_by(!!id_var) %>%
    arrange(!!time_var) %>%
    slice(1L) %>%
    ## change the baseline outcome_vars as new variable
    dplyr::select(baseline = !!outcome_var, everything()) %>%
    ## move the original time_var as all ZEROs
    dplyr::select(- !!time_var)

  ## the joint dataset from anchor prediction and baseline
  ## just join because there is one ID variables
  data_new <- full_join(data_anchor,
                        data_baseline,
                        by = join_by(!!id_var)) %>%
    ## add one factor time_var variable
    dplyr::mutate(!!time_var := as.factor(!!time_var)) %>%
    rename(!!outcome_var := `.pred`)

  ## add the attributes: model and dataset
  attr(data_new, "model") <- model_bks
  attr(data_new, "prediction") <- data_anchor
  attr(data_new, "baseline") <- data_baseline

  ## return the output as single dataframe
  return(data_new)
}

# Second level imputation ------------------------------------------------------
## 1.2 linear_impute ----------------------------------------------------------

#' Title Second Stage Model Linear Regression at anchor time points
#'
#' @description The function is used to impute the missing values after the brokenstick model;
#' the function will take the outcomes from the brokenstick model and impute the outcomes at the
#' anchor time points using a linear regression model. We will include extra time-invariant covariates
#' into the linear model and the baseline outcome (possibly with interaction terms).
#' Currently this function is only called in the single time point matching `people_like_i()` function.
#' @param lm_formula the second stage model with a user defined linear regression formula,
#' optional to include other time-invariant covariates and baseline outcome.
#' @param data_impute the training data set for the linear regression model,
#' which depends on the anchor time points
#' @param data_test the testing data set for the linear regression model,
#' which depends on the anchor time points
#' @param id_var the id variable name must be included in the dataset
#' @param outcome_var the outcome variable name must be included in the dataset
#' @param time_var the time variable name must be included in the dataset
#' @param anchor_time the anchor time set for imputation
#' @param ... Not used, but required for future extension
#'
#' @return A list with the imputed values for the training data set and testing data set
#' @export
#'
#' @examples \dontrun {}
#'
linear_impute <- function(lm_formula,
                          data_impute,
                          data_test,
                          id_var,
                          outcome_var,
                          time_var,
                          anchor_time,
                          ...) {
    outcome_var <- ensym(outcome_var)
    time_var <- ensym(time_var)
    id_var <- ensym(id_var)
    formula = paste0(lm_formula)

    lm_bks <- lm(as.formula(formula),
                 data = data_impute)
    # Tue Jul 25 22:30:34 2023 ------------------------------
    data_test[paste0("anchor_", anchor_time)] <- NA

    # View(data_test)
    data_test1 <- data_test %>%
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

    lp_train <- data_impute %>%
      ungroup() %>%
      mutate(lm_bks_target = predict(lm_bks)) %>%
      dplyr::select(!!id_var, !!time_var, contains("lm_bks_target")) %>%
      as.matrix() %>%
      as.data.frame() %>%
      rename(!!outcome_var := lm_bks_target)

    results <- list(testing = lp_test, training = lp_train, summary = summary(lm_bks))

    return(results)
    }

## 1.3 multiple_impute --------------------------------------------------------
#' Title Multiple Linear Regression imputation at anchor time points
#' @description The function is used to impute the missing values after the brokenstick model;
#' the function will take the outcomes from the brokenstick model and impute the outcomes at the
#' anchor time points using multiple linear regressions model at each anchor time point.
#' We will include extra time-invariant covariates. Different from `linear_impute()` function,
#' Here we fit multiple linear regression models at each anchor time point independently,
#' which grant more flexibility for the models.
#' This function is mainly used in `people-like-me()` and `people-like-us()` function,
#'  as an alternate to `linear_impute()`.
#'
#' @param lm_formula the second stage model with a user defined linear regression formula,
#' it depends on the liner model type and anchor time points used.
#' @param data_impute the design matrix for training data set for the linear regression model,
#' @param data_test the design matrix for testing data set for the linear regression model,
#' @param id_var the id variable name must be included in the dataset
#' @param outcome_var the outcome variable name must be included in the dataset
#' @param time_var the time variable name must be included in the dataset
#' @param anchor_time the anchor time set for imputation
#' @param ... Not used, but required for future extension
#'
#' @return A list with the imputed values for the training data set and testing data set,
#' and the summarization for the linear models probably will be removed in the furture.
#' @export
#'
#' @examples \dontrun {}
multiple_impute <- function(lm_formula,
                          data_impute,
                          data_test,
                          id_var,
                          outcome_var,
                          time_var,
                          anchor_time,
                          ...) {

  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)
  formula = paste0(lm_formula)

  lm_bks <- data_impute %>%
    group_by(time) %>%
    group_split() %>%
    map(~lm(as.formula(formula), .x))

  lp_test <- map_dfc(lm_bks, ~predict(., newdata = data_test) %>%
                   cbind())

  lp_train <- map_dfc(lm_bks, ~predict(.) %>%
                       cbind())

  results <- list(testing = lp_test,
                  training = lp_train,
                  summary = lm_bks)

  return(results)
}
























