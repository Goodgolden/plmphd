# First level imputation -------------------------------------------------------


## 1.1.1 impute_brokenstick -----------------------------------------------------
#' Title Imputation with `brokenstick::brokenstick()`
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
#' @return
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
  data_new <- full_join(data_anchor, data_baseline, by = join_by(!!id_var)) %>%
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

## 1.1.2 impute_kernel-----------------------------------------------------------
impute_kernel <- function(kernel,
                          time_var,
                          outcome_var,
                          ...) {

}


## 1.1.3 impute_random_walk -----------------------------------------------------
impute_rw2 <- function(...) {

}

# Second level imputation ------------------------------------------------------

## 1.2.1 linear_impute ----------------------------------------------------------

#' Title Linear regression for people-like-me methods
#'
#' @param lm_formula
#' @param data_impute
#' @param data_test
#' @param id_var
#' @param outcome_var
#' @param time_var
#' @param anchor_time
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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

    # browser()
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


multiple_impute <- function(lm_formula,
                          data_impute,
                          data_test,
                          id_var,
                          outcome_var,
                          time_var,
                          anchor_time,
                          ...) {
  # browser()
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
  
  results <- list(testing = lp_test, training = lp_train, summary = lm_bks)
  
  return(results)
}
























