## 11.1 right_rows -------------------------------------------------------------

## 11.2 IndvPred_lmer ----------------------------------------------------------
#' Title Individualized Predictions from Linear Mixed Models for `lme4::lmer()`
#' (Adapted from Dimitris Rizopoulos)
#'
#' @param lmerObject an object inheriting from function \code{lmer()},
#' class must be `lmerMod`.
#' @param data a data frame in which to train the linear mixed model
#'  (need to be updated in the plm methods).
#' @param newdata a data frame in which to look for variables with which to predict.
#' @param timeVar a character string indicating what type of intervals should be computed.
#' @param times a numeric vector denoting the time points for which we wish to compute the
#' subject-specific predictions after the last available measurement provided in
#' \code{newdata}. Default is a sequence of 100 equally spaced time points
#' from the smallest to the largest follow-up time of all subjects.
#' @param M numeric scalar denoting the number of Monte Carlo samples.
#' @param interval a character string indicating what type of intervals should be computed.
#' @param all_times logical; should predictions be calculated at all \code{times} or only
#' at the ones that are after the last observed time of each subject.
#' @param level a numeric scalar denoting the tolerance/confidence level.
#' @param return_data logical; if \code{TRUE} the data frame supplied in
#' \code{newdata} is returned augmented with the outputs of the function.
#' @param seed numeric scalar, the random seed used to produce the results.
#'
#' @return a data frame with the predicted values,
#' lower and upper bounds of the prediction intervals.
#'
#' If \code{return_data = TRUE},
#'  a the data frame \code{newdata} with extra rows for the time points at
#'  which predictions were calculated, and extra columns with the predictions
#'  and the limits of the pointwise confidence intervals.
#'
#'  If \code{return_data = FALSE}, a list with components
#'  \item{times_to_pred}{time points at which predictions were calculated.}
#'  \item{predicted_y}{the predictions.}
#'  \item{low}{the lower limits of the pointwise confidence intervals.}
#'  \item{upp}{the upper limits of the pointwise confidence intervals.}
#'
#' @export
#'
#' @examples {
#' \dontrun{
#' # linear mixed model fit
#'  lmer <- lmer()
#'
#'  id2 <- IndvPred_lmer(lmer,
#'                       newdata = subset(tsa_test1, id == 2),
#'                       timeVar = "time",
#'                       M = 1000,
#'                       return_data = TRUE)
#'   }
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

