## 11.2 IndvPred_lmer ----------------------------------------------------------
#' Individualized Predictions from Linear Mixed Models for `lme4::lmer()`
#'
#' @description This function provides subject-specific predictions from a fitted
#' `lmerMod` object (created by \code{lme4::lmer()}). It calculates the predictions
#' using Monte Carlo sampling, optionally generating prediction intervals,
#' and offers flexibility for predicting at all specified time points or only
#' after the last observed time for each subject.
#'
#' @param lmerObject An object of class `lmerMod` resulting from \code{lmer()}.
#' @param data A data frame used to train the linear mixed model.
#' @param newdata A data frame containing the variables for the new subjects
#' for which predictions are needed.
#' @param timeVar A character string specifying the time variable in the data.
#' @param outcomeVar A character string specifying the outcome variable.
#' @param idVar A character string specifying the ID variable representing individuals.
#' @param times A numeric vector specifying the time points for which
#' subject-specific predictions should be calculated. Defaults to
#' 100 equally spaced time points.
#' @param lmer.fixed The formula for the fixed effects part of the model.
#' @param lmer.random The formula for the random effects part of the model.
#' @param M An integer specifying the number of Monte Carlo samples. Default is 200.
#' @param interval A character string indicating the type of intervals to compute:
#' either "confidence" or "prediction". Defaults to "confidence".
#' @param all_times Logical; if \code{TRUE}, predictions are computed at all
#' time points in \code{times}, otherwise only after the last observed time for each subject.
#' @param level A numeric scalar specifying the confidence level for the prediction intervals. Default is 0.95.
#' @param return_data Logical; if \code{TRUE}, the function returns the data frame supplied in
#' \code{newdata} augmented with the predictions and intervals. If \code{FALSE},
#' a list with predicted values and intervals is returned.
#' @param seed An integer used to set the random seed for reproducibility. Default is 1.
#' @param ... Additional arguments passed to the function.
#'
#' @return A data frame or list with the following components:
#' \itemize{
#'   \item If \code{return_data = TRUE}, a data frame \code{newdata} with additional rows for
#'   the time points where predictions were made, and additional columns for the predicted values
#'   and confidence/prediction intervals.
#'   \item If \code{return_data = FALSE}, a list with:
#'     \item{times_to_pred}{Time points where predictions were made.}
#'     \item{predicted_y}{The predicted values at these time points.}
#'     \item{low}{The lower bounds of the prediction intervals.}
#'     \item{upp}{The upper bounds of the prediction intervals.}
#' }
#' @note This function is adapted from the \code{IndvPred_lme()} function in the \code{JMbayes} package. Please see the \code{JMbayes} package for more details.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example with lmer:
#'   library(lme4)
#'   lmer_mod <- lmer(y ~ time + (1 | id), data = example_data)
#'
#'   # Individualized prediction for subject with id = 2:
#'   pred_result <- IndvPred_lmer(lmerObject = lmer_mod,
#'                                data = example_data,
#'                                newdata = subset(example_data, id == 2),
#'                                timeVar = "time",
#'                                outcomeVar = "y",
#'                                idVar = "id",
#'                                M = 1000,
#'                                return_data = TRUE)
#' }
IndvPred_lmer <- function (lmerObject,
                           data,
                           newdata,
                           timeVar,
                           outcomeVar,
                           idVar,
                           times = NULL,
                           lmer.fixed = "1 + bs(time, knots = c(5, 10, 15), degree = 1)",
                           lmer.random = "1 + bs(time, knots = c(5, 10, 15), degree = 1)",
                           M = 200L,
                           interval = c("confidence", "prediction"),
                           all_times = TRUE,
                           level = 0.95,
                           return_data = TRUE,
                           seed = 1L,
                           ...) {
  interval <- match.arg(interval)
  call <- match.call()
  timeVar <- ensym(timeVar)
  outcomeVar <- ensym(outcomeVar)
  lmer.fixed  <- ensym(lmer.fixed)
  lmer.random  <- ensym(lmer.random)
  idVar <- ensym(idVar)

  if (inherits(lmerObject, "lmerMod")) {
    ## extract data.matrix info from lmerObject
    lfixed <- paste0(outcomeVar, "~", lmer.fixed)
    formYx <- formula(lfixed,
                      fixed.only = TRUE)
    ## below are the design matrix
    mfX <- model.frame(terms(formYx),
                       data = data)
    TermsX <- attr(mfX, "terms")

    lrandom <- paste0(outcomeVar, "~", lmer.random)
    formYz <- formula(lrandom,
                      fixed.only = TRUE)

    mfZ <- model.frame(terms(formYz),
                       data = data)
    TermsZ <- attr(mfZ, "terms")

    ## extract fixed and random effects
    idVar <- names(lmerObject@cnms)
    betas <- fixef(lmerObject)
    # betas
    sigma <- sigma(lmerObject)
    # sigma
    V <- vcov(lmerObject)
    D <- (VarCorr(lmerObject))
    D <- as.matrix(Matrix::bdiag(D))

    ## extract time and newdata
    times_orig <- data[[timeVar]]
    times_orig <- times_orig[!is.na(times_orig)]
  }

  ## rebuild for the new
  all_vars <- unique(c(all.vars(TermsX),
                       all.vars(TermsZ)))
  ## remove the missing values
  ## Mon Apr 22 09:49:17 2024 --------------------------------------------------
  ## better just use the complete dataset at the first step
  newdata_nomiss <- newdata[complete.cases(newdata[all_vars]), ]
  ## build new data.frame for prediction
  mfX_new <- model.frame(TermsX,
                         data = newdata_nomiss)
  X_new <- model.matrix(formYx,
                        mfX_new)
  mfZ_new <- model.frame(TermsZ,
                         data = newdata_nomiss)
  Z_new <- model.matrix(formYz,
                        mfZ_new)
  na_ind <- attr(mfX_new, "na.action")
  y_new <- model.response(mfX_new,
                          "numeric")

  # browser()
  if (length(idVar) > 1)
    stop("the current version of the function only works
         with a single grouping variable.\n")
  if (is.null(newdata[[idVar]]))
    stop("subject id variable not in newdata.\n")

  # Mon Apr 22 09:51:49 2024 ------------------------------
  ## might need to fix this, have no idea why drizopoulos use this
  ## otherwise the function will not work for match()
  # id_nomiss <- match(newdata_nomiss[[idVar]], unique(newdata_nomiss[[idVar]]))
  newdata_nomiss[[idVar]] <- as.factor(newdata_nomiss[[idVar]])
  # id_nomiss <- unique(newdata_nomiss[[idVar]])
  id_nomiss <- base::match(newdata_nomiss[[idVar]],
                           unique(newdata_nomiss[[idVar]]))
  n <- length(unique(id_nomiss))

  ## empty vectors for the prediction
  modes <- matrix(0, n, ncol(Z_new))
  post_vars <- DZtVinv <- vector("list", n)

  for (i in seq_len(n)) {
    ## this is the most weird part of the function
    ## why use the boolean to calculate the matrix
    ## probably need to check more than one time for prediction
    id_i <- id_nomiss == i
    X_new_id <- X_new[id_i, , drop = FALSE]
    Z_new_id <- Z_new[id_i, , drop = FALSE]
    Vi_inv <- solve(Z_new_id %*% tcrossprod(D, Z_new_id) + sigma^2 * diag(sum(id_i)))
    DZtVinv[[i]] <- tcrossprod(D, Z_new_id) %*% Vi_inv
    modes[i, ] <- c(DZtVinv[[i]] %*% (y_new[id_i] - X_new_id %*% betas))
    t1 <- DZtVinv[[i]] %*% Z_new_id %*% D
    t2 <- DZtVinv[[i]] %*% X_new_id %*% V %*%
      crossprod(X_new_id, Vi_inv) %*% Z_new_id %*% D
    post_vars[[i]] <- D - t1 + t2
  }
  fitted_y <- c(X_new %*% betas) +
    ## conditional random effects based on modes
    rowSums(Z_new %*% t(modes[id_nomiss, , drop = FALSE]))

  if (is.null(times) || !is.numeric(times)) {
    times <- seq(min(times_orig),
                 max(times_orig),
                 length.out = 100)
  }
  id <- unique(newdata[[idVar]])
  newdata$time <- newdata[[timeVar]]
  last_time <- newdata %>%
    group_by(!!sym(idVar)) %>%
    summarize(last_time = max(time)) %>%
    dplyr::select(last_time) %>%
    unlist()

  # Mon Apr 22 10:01:28 2024 ------------------------------
  ## we have to predict for every time for every individual
  ## this part is better to be checked
  ## whether we can only predict the time for one individual?
  times_to_pred <- lapply(last_time,
                          function(t) if (all_times)
                            times
                          else times[times > t])

  id_pred <- rep(seq_len(n), sapply(times_to_pred, length))

  ## right row have not idea
  ##  what is that check the original package
  ## right_rows() --------------------------------------------------------------
  right_rows <- function(data, times, ids, Q_points) {

    ## so there are 913 fids
    fids <- factor(ids, levels = unique(ids))

    if (!is.list(Q_points))
      Q_points <- base::split(Q_points, row(Q_points))

    ## need to be deleted
    ## the total rows for the newdata set is 7374
    bst1 <- split(newdata, newdata$id) %>%
      map("time")
    ind <- mapply(findInterval, Q_points, bst1)
    ind[ind < 1] <- 1
    rownams_id <- split(row.names(data), newdata$id)
    ind <- mapply(`[`, rownams_id, split(ind, col(ind)))
    data[c(ind), ]
  }


  newdata_pred <- right_rows(newdata, newdata[[timeVar]],
                             id, times_to_pred)

  newdata_pred[[timeVar]] <- unlist(times_to_pred)

  ## final desgin matrix for the prediction
  mfX_new_pred <- model.frame(TermsX,
                              data = newdata_pred,
                              na.action = NULL)
  X_new_pred <- model.matrix(formYx, mfX_new_pred)
  mfZ_new_pred <- model.frame(TermsZ,
                              data = newdata_pred,
                              na.action = NULL)
  Z_new_pred <- model.matrix(formYz, mfZ_new_pred)

  predicted_y <- c(X_new_pred %*% betas) +
    rowSums(Z_new_pred * modes[id_pred, , drop = FALSE])

  set.seed(seed)
  betas_M <- MASS::mvrnorm(M, betas, V)
  modes_fun <- function (betas) {
    t(mapply("%*%", DZtVinv, split(y_new - X_new %*% betas, id_nomiss)))
  }
  modes_M <- lapply(split(betas_M, row(betas_M)), modes_fun)
  matrix_row <- function (m, i) m[i, , drop = FALSE]
  modes_M <- lapply(seq_len(n), function (i) t(sapply(modes_M, matrix_row, i = i)))
  b_M <- modes_M
  for (i in seq_len(n)) {
    b_M[[i]] <- t(apply(modes_M[[i]], 1, MASS::mvrnorm, n = 1, Sigma = post_vars[[i]]))
  }

  n_pred <- length(predicted_y)
  sampled_y <- matrix(0, n_pred, M)

  for (m in seq_len(M)) {
    betas_m <- betas_M[m, ]
    b_m <- t(sapply(b_M,
                    function(x) x[m, ]))
    mean_m <- c(X_new_pred %*% betas_m) +
      rowSums(Z_new_pred *
                b_m[id_pred, ,
                    drop = FALSE])
    sampled_y[, m] <- if (interval == "confidence")
      mean_m
    else rnorm(n_pred, mean_m,
               sigma(lmerObject))
  }

  low <- apply(sampled_y,
               1,
               quantile,
               probs = (1 - level) / 2)
  upp <- apply(sampled_y,
               1,
               quantile,
               probs = 1 - (1 - level) / 2)
  rm(list = ".Random.seed",
    envir = globalenv())


  if (!return_data) {
    out_data <- cbind(
      id = rep(unique(newdata_pred[[idVar]]),
               each = length(times)),
      time = times,
      predicted_y = predicted_y
      # low = low,
      # upp = upp
    )
  } else {
    out_data <- rbind(newdata,
                      newdata_pred)
    out_data$pred <- c(fitted_y,
                       predicted_y)
    out_data$low <- c(rep(NA, length(fitted_y)),
                      low)
    out_data$upp <- c(rep(NA, length(fitted_y)),
                      upp)
    out_data[order(out_data[[idVar]],
                   out_data[[timeVar]]), ]
  }

  return(out_data)
}

