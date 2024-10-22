# First level imputation -------------------------------------------------------

## 1.1 impute_brokenstick -----------------------------------------------------
#' First Stage Model Imputation with `brokenstick::brokenstick()` at Anchor Time Points
#'
#' @description
#' This function uses the `brokenstick::brokenstick()` model to impute values at specified anchor time points in a longitudinal dataset. The predictions from the broken stick model represent the group-conditional means of the random effects. The user can calculate prediction (imputation) at given anchor time points using specified outcome, time, and ID variables.
#'
#' @param outcome_var \code{character}. The name of the outcome variable in the dataset.
#' @param time_var \code{character}. The name of the time variable in the dataset.
#' @param id_var \code{character}. The name of the ID variable in the dataset.
#' @param bs_knots \code{numeric vector}. The internal knots for the brokenstick model.
#' @param anchor_time \code{numeric}. The anchor time point(s) for imputation.
#' @param data \code{data.frame}. A longitudinal dataset in long format, containing the specified variables.
#' @param ... Not used, but required for future extension.
#'
#' @return \code{data.frame}. A data frame with the imputed values at the anchor time points, baseline information, and the fitted model.
#'
#' @details
#' The function fits the brokenstick model based on the provided outcome, time, and ID variables, and then predicts the outcome variable at the given anchor time points. The returned data frame contains the imputed values, the baseline outcome values, and the original dataset.
#' The attributes of the returned data frame include:
#' \describe{
#'   \item{\code{model}}{The fitted brokenstick model object.}
#'   \item{\code{prediction}}{The predicted values from the model at the anchor time points.}
#'   \item{\code{baseline}}{The baseline values for the outcome variable.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example of using the function
#' # Sample data preparation
#' library(dplyr)
#' data <- data.frame(
#'   id = rep(1:10, each = 5),
#'   time = rep(seq(0, 10, by = 2.5), 10),
#'   outcome = rnorm(50)
#' )
#'
#' # Define the parameters
#' outcome_var <- "outcome"
#' time_var <- "time"
#' id_var <- "id"
#' bs_knots <- c(2.5, 5, 7.5)
#' anchor_time <- c(3, 6, 9)
#'
#' # Call the impute_brokenstick function
#' result <- impute_brokenstick(
#'   outcome_var = outcome_var,
#'   time_var = time_var,
#'   id_var = id_var,
#'   bs_knots = bs_knots,
#'   anchor_time = anchor_time,
#'   data = data
#' )
#'
#' # Inspect the result
#' head(result)
#' attr(result, "model")   # Access the fitted model
#' attr(result, "baseline") # Access the baseline data
#' }

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

#' Second Stage Model Linear Regression at Anchor Time Points
#'
#' @description This function performs a second-stage imputation using linear regression at anchor time points, after the first-stage imputation with the brokenstick model. It takes the imputed outcomes from the brokenstick model and performs imputation for missing values at anchor time points using a linear regression model. The model can include time-invariant covariates and baseline outcomes, possibly with interaction terms. This function is primarily called within the `people_like_i()` function for single time point matching.
#'
#' @param lm_formula \code{formula}. A user-defined linear regression formula for the second-stage imputation. The formula can optionally include additional time-invariant covariates and baseline outcomes.
#' @param data_impute \code{data.frame}. The training dataset used to fit the linear regression model, based on the anchor time points.
#' @param data_test \code{data.frame}. The testing dataset for which missing values are imputed using the linear regression model.
#' @param id_var \code{character}. The name of the ID variable in the dataset.
#' @param outcome_var \code{character}. The name of the outcome variable in the dataset.
#' @param time_var \code{character}. The name of the time variable in the dataset.
#' @param anchor_time \code{numeric}. The anchor time point(s) at which imputation is performed.
#' @param ... Additional arguments, currently not used but included for future extensions.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{testing}}{\code{data.frame}. The testing dataset with imputed values at the anchor time points.}
#'   \item{\code{training}}{\code{data.frame}. The training dataset with imputed values at the anchor time points.}
#'   \item{\code{summary}}{\code{summary.lm}. A summary of the fitted linear regression model.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' # Prepare sample data
#' data_impute <- data.frame(
#'   id = rep(1:10, each = 5),
#'   time = rep(seq(0, 10, by = 2.5), 10),
#'   outcome = rnorm(50),
#'   covariate = runif(50)
#' )
#' data_test <- data_impute
#'
#' # Define parameters
#' lm_formula <- "outcome ~ time + covariate"
#' id_var <- "id"
#' outcome_var <- "outcome"
#' time_var <- "time"
#' anchor_time <- 5
#'
#' # Call the linear_impute function
#' result <- linear_impute(
#'   lm_formula = lm_formula,
#'   data_impute = data_impute,
#'   data_test = data_test,
#'   id_var = id_var,
#'   outcome_var = outcome_var,
#'   time_var = time_var,
#'   anchor_time = anchor_time
#' )
#'
#' # Inspect the results
#' head(result$testing)  # Testing dataset with imputed values
#' head(result$training) # Training dataset with imputed values
#' print(result$summary) # Summary of the linear regression model
#' }

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
#' Multiple Linear Regression Imputation at Anchor Time Points
#'
#' @description This function is used to impute missing values after the first-stage imputation with the brokenstick model. It takes the imputed outcomes from the brokenstick model and imputes outcomes at anchor time points using multiple linear regression models. A separate linear regression model is fitted at each anchor time point, allowing for greater flexibility. Time-invariant covariates can also be included in the model. This function is mainly used within `people-like-me()` and `people-like-us()` as an alternative to `linear_impute()`.
#'
#' @param lm_formula \code{formula}. A user-defined linear regression formula for the second-stage imputation. The formula can vary depending on the linear model type and the anchor time points used.
#' @param data_impute \code{data.frame}. The design matrix for the training dataset used to fit the linear regression models.
#' @param data_test \code{data.frame}. The design matrix for the testing dataset for which missing values are imputed using the linear regression models.
#' @param id_var \code{character}. The name of the ID variable in the dataset.
#' @param outcome_var \code{character}. The name of the outcome variable in the dataset.
#' @param time_var \code{character}. The name of the time variable in the dataset.
#' @param anchor_time \code{numeric}. The anchor time points at which imputation is performed.
#' @param ... Additional arguments, currently not used but included for future extensions.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{testing}}{\code{data.frame}. The testing dataset with imputed values at the anchor time points.}
#'   \item{\code{training}}{\code{data.frame}. The training dataset with imputed values at the anchor time points.}
#'   \item{\code{summary}}{\code{list}. A list of fitted linear models for each anchor time point.}
#' }
#'
#' @details Each anchor time point is treated independently, and a linear regression model is fitted at each anchor time point separately. This method offers more flexibility than the `linear_impute()` function, where a single linear model is fitted across time points.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define the necessary parameters
#' lm_formula <- "outcome ~ time + covariate"
#' id_var <- "id"
#' outcome_var <- "outcome"
#' time_var <- "time"
#' anchor_time <- c(3, 6, 9)  # Example anchor times
#'
#' # Call the multiple_impute function
#' result <- multiple_impute(
#'   lm_formula = lm_formula,
#'   data_impute = data_impute,  # Assumed pre-defined data
#'   data_test = data_test,      # Assumed pre-defined data
#'   id_var = id_var,
#'   outcome_var = outcome_var,
#'   time_var = time_var,
#'   anchor_time = anchor_time
#' )
#'
#' # Access the imputed values and model summaries
#' head(result$testing)   # Imputed testing dataset
#' head(result$training)  # Imputed training dataset
#' summary(result$summary[[1]]) # Summary of the first linear model
#' }

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
























