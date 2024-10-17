brokenstick <- function(formula,
                        data,
                        knots = NULL,
                        boundary = NULL,
                        k = 5L,
                        method,
                        degree = 1L,
                        control = set_control(method = method, ...),
                        na.action = na.exclude,
                        light = FALSE,
                        hide = c("right", "left", "boundary", "internal", "none"),
                        ...) {
  call <- match.call()
  stopifnot(
    inherits(formula, "formula"),
    is.data.frame(data) || is.matrix(data),
    as.integer(degree) %in% c(0L, 1L)
  )

  data <- data.frame(data)
  method <- match.arg(method)
  hide <- match.arg(hide)

  obj <- brokenstick_bridge(
    formula, data, knots, boundary, k, degree,
    method, control, na.action, light, hide, call,
    ...
  )
  return(obj)
}

brokenstick_bridge <- function(formula,
                               data,
                               knots,
                               boundary,
                               k,
                               degree,
                               control,
                               na.action,
                               light,
                               hide,
                               call,
                               warn_splines = FALSE,
                               ...) {

  browser()
  names <- parse_formula(formula)
  nms <- unname(unlist(names))


  if (!all(nms %in% colnames(data))) {
    stop("Variable(s) not found: ",
         paste(nms[!nms %in% colnames(data)], collapse = ", "),
         call. = FALSE
    )
  }

  y <- data[[names[["y"]]]]
  x <- data[[names[["x"]]]]
  g <- data[[names[["g"]]]]

  stopifnot(
    is.numeric(y),
    is.numeric(x),
    is.numeric(g) || is.factor(g) || is.character(g)
  )

  l <- calculate_knots(x, k, knots, boundary)
  X <- make_basis(x,
                  xname = names$x,
                  internal = l$internal,
                  boundary = l$boundary,
                  degree = degree,
                  warn = warn_splines)

  data_pad <- data.frame(data,
                         X,
                         stringsAsFactors = FALSE)
  names(data_pad) <- c(names(data),
                       colnames(X))
  pred <- paste("0 +",
                paste(colnames(X),
                      collapse = " + "))
  fm <- as.formula(paste(names$y, "~",
                         pred, "+ (",
                         pred, "|",
                         names$g, ")"))
  fit <- brokenstick_impl_lmer(
    data = data_pad,
    formula = fm,
    control = control,
    na.action = na.action
  )


  obj <- new_brokenstick(
    call = call,
    names = names,
    internal = l$internal,
    boundary = l$boundary,
    degree = degree,
    method = method,
    control = control,
    beta = fit$beta,
    omega = fit$omega,
    sigma2j = fit$sigma2j,
    sigma2 = fit$sigma2,
    light = light,
    hide = hide,
    data = data,
    sample = fit$sample,
    imp = fit$imp,
    mod = fit$mod
  )
  return(obj)
}

brokenstick_impl_lmer <- function(data,
                                  formula,
                                  control,
                                  na.action) {
  # Bates et al, linear mixed-effects model
  # problem ------------------------------------------
  # what do we do with the control
  # why there is always singularity issue here?
  mod <- lmer(formula = formula,
              data = data,
              control = control,
              na.action = na.action)

  # Here we trust that names(slot(model, "cnms"))
  # gives the name of the
  # group variable
  df <- as.data.frame(VarCorr(mod))
  y <- mod@resp$y
  obj <- list(
    mod = mod,
    beta = fixef(mod),
    omega = as.matrix(as.data.frame(VarCorr(mod)[[names(slot(mod, "cnms"))]])),
    sigma2j = numeric(),
    sigma2 = df[df$grp == "Residual", "vcov"],
    sample = c(
      length(y),
      sum(!is.na(y)),
      sum(is.na(y)),
      as.integer(ngrps(mod)),
      0L
    )
  )
  return(obj)
}

# Define the brokenstick object -----------------------------------------------
#' Class `brokenstick`
#'
#' The main fitting function [brokenstick()] returns an object of class
#' `brokenstick`. This object collects the fitted broken stick model.
#'
#' The package exports S3 methods for the `brokenstick` class for the following
#' generic functions: [coef()], [fitted()], [model.frame()], [model.matrix()],
#' [plot()], [predict()], [print()], [residuals()] and [summary()].
#'
#' The package exports the following helper functions for `brokenstick` objects:
#' [get_knots()], [get_omega()] and [get_r2()].
#'
#' A `brokenstick` object is a list with the following named elements:
#'
#' @section Elements:
#' \describe{
#'    \item{`call`}{Call that created the object}
#'    \item{`names`}{A named list with three elements (`"x"`, `"y"`, `"g"`)
#'    providing the variable name for time, outcome and subject, respectively.}
#'    \item{`internal`}{Numeric vector of with internal knots.}
#'    \item{`boundary`}{Numeric vector of length 2 with the boundary knots.}
#'    \item{`degree`}{The `degree` of the B-spline. See [splines::bs()]. Support
#'    only the values of 0 (step model) or 1 (broken stick model).}
#'    \item{`method`}{String, either `"kr"` or `"lmer"`, identifying the fitting model.}
#'    \item{`control`}{List of control options returned by [set_control()] used
#'    to set algorithmic details.}
#'    \item{`beta`}{Numeric vector with fixed effect estimates.}
#'    \item{`omega`}{Numeric matrix with variance-covariance estimates of the
#'    broken stick estimates.}
#'    \item{`sigma2`}{Numeric scalar with the mean residual variance.}
#'    \item{`sample`}{A numeric vector with descriptives of the training data.}
#'    \item{`light`}{Should the returned object be lighter? If `light = TRUE`
#'    the returned object will contain only the model settings and parameter
#'    estimates and not store the `sigma2j`, `sample`, `data`, `imp` and
#'    `mod` elements.
#'    The light object can be used to predict broken stick estimates for
#'    new data, but does not disclose the training data and is small.}
#'    \item{`hide`}{Should the output for boundary knots be hidden? Can
#'    be `"right"`, `"left"`, `"boundary"`, `"internal"` or `"none"`.
#'    The default is `"right"`.}
#'    \item{`sigma2j`}{Numeric vector with estimates of the residual variance per
#'    group. Only used by method `"kr"`.}
#'    \item{`data`}{The training data used to fit the model.}
#'    \item{`imp`}{The imputations generated for the missing outcome data. Only
#'    for `method = "kr"`.}
#'    \item{`mod`}{For `method = "kr"`: A named list with four components, each of
#'    class [coda::mcmc]. For `method = "lmer"`: An object of class
#'    [lme4::merMod-class].}
#' }
#'
#' @name brokenstick-class
#' @rdname brokenstick-class
#' @author Stef van Buuren 2023

new_brokenstick <- function(call = match.call(),
                            names = list(x = character(),
                                         y = character(),
                                         g = character()),
                            internal = numeric(0),
                            boundary = numeric(0),
                            degree = 1L,
                            method = NA_character_,
                            control = list(),
                            beta = numeric(0),
                            omega = numeric(0),
                            sigma2j = numeric(0),
                            sigma2 = numeric(0),
                            light = FALSE,
                            hide = NA_character_,
                            sample = numeric(0),
                            data = numeric(0),
                            imp = numeric(0),
                            mod = list()) {
  ## Here is how you write a object
  ## first just write out every single element you
  ## want to return from the function to the end
  result <- list(call = call,
                 names = names,
                 internal = internal,
                 boundary = boundary,
                 degree = degree,
                 method = method,
                 control = control,
                 beta = beta,
                 omega = omega,
                 sigma2 = sigma2,
                 sample = sample,
                 light = light,
                 hide = hide)

  if (!light) {
    result$sigma2j <- sigma2j
    result$data <- data
    result$imp <- imp
    result$mod <- mod
  }

  class(result) <- "brokenstick"
  return(result)
}

# The main brokenstick package -------------------------------
## These functions are used to lmer objects and to extract the random effects

#' Fit a `brokenstick` model to irregular data (from brokenstick package)
#'
#' The `brokenstick()` function fits an irregularly observed series
#' of measurements onto a user-specified grid of points (knots).
#' The model codes the grid by a series of linear B-splines.
#' Each modelled trajectory consists of straight lines that join at
#' the chosen knots and look like a broken stick. Differences between
#' observations are expressed by a random effect per knot.
#'
#' @param formula A formula specifying the outcome, the predictor and the group
#' variable in `data`. The generic shape is `formula = y ~ x | group`. The
#' left-hand side is the outcome, the right-hand side the predictor, and the
#' name of the grouping variable occurs after the `|` sign. Formula treatment
#' is non-standard: 1) `y` and `x` should be numeric, 2) only one variable
#' is allowed in each model term (additional variables will be ignored).
#'
#' @param data A data frame or matrix containing the outcome (numeric),
#' predictor (numeric) and group (numeric, factor, character) variable.
#'
#' @param knots Optional, but recommended. Numerical vector with the
#' locations of the internal knots to be placed on the values of the predictor.
#' The function sorts the internal knots in increasing order.
#'
#' @param boundary Optional, but recommended. Numerical vector of
#' length 2 with the left and right boundary knot. The `boundary`
#' setting is passed to [splines::bs()] as the `Boundary.knots` argument.
#' If not specified, the function determines the boundary knots as
#' `range(x)`. When specified, the `boundary` range is internally
#' expanded to include at least `range(knots)`.
#'
#' @param k Optional, a convenience parameter for the number of
#' internal knots. If specified, then `k` internal knots are placed
#' at equidense quantiles of the predictor. For example,
#' specifying `k = 1` puts a knot at the 50th quantile (median),
#' setting `k = 3` puts knots at the 25th, 50th and 75th quantiles,
#' and so on. If the user specifies both `k` and `knots` arguments
#' then `knots` takes precedence. The default is `k = 5`, so if the user
#' does not specify any of `knots`, `boundary` or `k`, then the knots
#' will be at the 16th, 33th, 50th, 66th and 84th quantile of the
#' predictor.
#'
#' @param degree the degree of the spline. The broken stick model
#' requires linear splines, so the default is `degree = 1`.
#' Setting `degree = 0` yields (crisp) dummy coding, and one
#' column less than for `degree = 1`. The `brokenstick` package supports
#' only `degree = 0` and `degree = 1`.
#'
#' @param method Estimation method. Either `"kr"` (for the
#' Kasim-Raudenbush sampler) or `"lmer"` (for [lme4::lmer()]).
#' Version 1.1.1.9000 changed the default to `method = "kr"`.
#'
#' @param control List of control options returned by [set_control()] used
#'    to set algorithmic details. A list with parameters. When not specified,
#'    the functions sets to defaults
#'    for method `"kr"` by [brokenstick::control_kr()], and
#'    for method `"lmer"` by [lme4::lmerControl()]. For ease of use, the user
#'    may set individual options to `"kr"` (e.g. `niter = 500`) via the \dots
#'    arguments.
#'
#' @param na.action A function that indicates what [lme4::lmer()] should so
#' when the data contain `NA`s. Default set to `na.exclude`. Only used by
#' method `"lmer"`.
#'
#' @param light Should the returned object be lighter? If `light = TRUE`
#'    the returned object will contain only the model settings and parameter
#'    estimates and not store the `data`, `imp` and `mod` elements. The light
#'    object can be used to predict broken stick estimates for new data, but
#'    does not disclose the training data and is very small (often <20 Kb).
#'
#' @param hide Should output for knots be hidden in get, print, summary and plot
#' functions? Can be `"left"`, `"right"`, `"boundary"`, `"internal"` or `"none"`.
#' The default is `"right"`.
#'
#' @param \dots Forwards arguments to [brokenstick::control_kr()].
#'
#' @note
#' Note that automatic knot specification is data-dependent, and may not reproduce
#' on other data. Likewise, knots specified via `k` are data-dependent and do not transfer
#' to other  data sets. Fixing the model requires specifying both `knots` and
#' `boundary`.
#'
#' @details
#' The choice between `method = "kr"` and `method = "lmer"` depends on the size
#' of the data and the complexity of the model. In general, setting `method = "lmer"`
#' can require substantial calculation time for more complex models
#' (say > 8 internal knots) and may not converge. Method `"kr"` is less
#' sensitive to model complexity and small samples, and has the added benefit that the
#' variance-covariance matrix of the random effects can be constrained through the
#' `cormodel` argument. On the other hand, `"lmer"` is the better-researched
#' method, and is more efficient for simpler models and datasets with many
#' rows.
#'
#' The default algorithm since version 2.0 is the Bayesian Kasim-Raudenbush
#' sampler (`method = "kr"`). The variance-covariance matrix of the broken stick
#' estimates absorbs the relations over time. The `"kr"` method allows
#' enforcing a simple structure on this variance-covariance matrix. Currently,
#' there are three such correlation models: `"none"` (default), `"argyle"`
#' and `"cole"`. Specify the `seed` argument for reproducibility.
#' See [control_kr()] for more details.
#'
#' The alternative `method = "lmer"` fits the broken stick model by
#' [lme4::lmer()]. With this method, the variance-covariance matrix can only be
#' unstructured. This estimate may be unstable if the number of children is
#' small relative to the number of specified knots. The default setting
#' in [lme4::lmerControl()] is  `check.nobs.vs.nRE= "stop"`. The
#' `[set_control()]` function changes this to `check.nobs.vs.nRE= "warning"`
#' by default, since otherwise many broken stick models would not run at all.
#' The method throws warnings that estimates are not stable. It can be time
#' for models with many internal knots. Despite the warnings,
#' the results often look reasonable.
#'
#' Diagnostics with \pkg{coda} and \pkg{lme4}: The function returns an object
#' of class `brokenstick`. For `method = "kr"` the list component named
#' `"mod"` contains a list of `mcmc` objects that can be further analysed with
#' [coda::acfplot()], [coda::autocorr()], [coda::crosscorr()], [coda::cumuplot()],
#' [coda::densplot()], [coda::effectiveSize()], [coda::geweke.plot()],
#' [coda::raftery.diag()], [coda::traceplot()] and the usual `plot()`
#' and `summary()` functions. For `method = "lmer"` the list component named
#' `"mod"` contains an object of class [lme4::merMod]. These model objects
#' are omitted in light `brokenstick` objects.
#' @return A object of class `brokenstick`.
#' @export
#' @examples
#' \dontrun{
#'geocode("3817 Spruce St, Philadelphia, PA 19104")
#'geocode("Philadelphia, PA")
#'dat <- data.frame(value=runif(3),address=c("3817 Spruce St, Philadelphia, PA 19104","Philadelphia, PA","Neverneverland"))
#'geocode(dat)
#'}

brokenstick <- function(formula,
                        data,
                        knots = NULL,
                        boundary = NULL,
                        k = 5L,
                        degree = 1L,
                        method = c("kr", "lmer"),
                        control = set_control(method = method, ...),
                        na.action = na.exclude,
                        light = FALSE,
                        hide = c("right", "left", "boundary", "internal", "none"),
                        ...) {
  call <- match.call()
  ## conditions should have been checked in every of funs
  ## in the package, need to read more for these setups
  stopifnot(inherits(formula, "formula"),
            is.data.frame(data) || is.matrix(data),
            as.integer(degree) %in% c(0L, 1L))
  data <- data.frame(data)
  method <- match.arg(method)
  hide <- match.arg(hide)

  obj <- brokenstick_bridge(formula,
                            data,
                            knots,
                            boundary,
                            k,
                            degree,
                            method,
                            control,
                            na.action,
                            light,
                            hide,
                            call,
                            ...)
  browser()
  formbs <- formula

  data1_ci50 <-
    IndvPred_lmer(lmerObject = obj$mod,
                  formbs = formbs,
                  data = train2,
                  newdata = train2 %>% group_by(id) %>% slice(4),
                  timeVar = "time",
                  M = 500,
                  times = anchor,
                  all_times = TRUE,
                  return_data = TRUE,
                  level = 0.50,
                  interval = "prediction",
                  seed = 555)

  data2_test <-
    IndvPred_lmer(lmerObject = obj$mod,
                  data = train2,
                  newdata = test2 %>% group_by(id) %>% slice(5),
                  timeVar = "time",
                  M = 1000,
                  times = anchor,
                  all_times = TRUE,
                  return_data = TRUE,
                  level = 0.50,
                  interval = "prediction",
                  seed = 555) %>%
    filter(time %in% anchor)
  return(obj)
}

## brokenstick_bridge called in brokenstick ------------------------------------
brokenstick_bridge <- function(formula,
                               data,
                               newdata,
                               knots,
                               boundary,
                               k,
                               degree,
                               method,
                               control,
                               na.action,
                               light,
                               hide,
                               call,
                               warn_splines = FALSE,
                               ...) {
  names <- parse_formula(formula)
  nms <- unname(unlist(names))
  if (!all(nms %in% colnames(data))) {
    stop("Variable(s) not found: ",
         paste(nms[!nms %in% colnames(data)], collapse = ", "),
         call. = FALSE
    )
  }

  y <- data[[names[["y"]]]]
  x <- data[[names[["x"]]]]
  g <- data[[names[["g"]]]]

  stopifnot(
    is.numeric(y),
    is.numeric(x),
    is.numeric(g) || is.factor(g) || is.character(g)
  )

  l <- calculate_knots(x, k, knots, boundary)
  X <- make_basis(x,
                  xname = names$x,
                  internal = l$internal,
                  boundary = l$boundary,
                  degree = degree,
                  warn = warn_splines
  )

  if (method == "kr") {
    fit <- kr(y = y, x = X, g = g, control = control)}

  if (method == "lmer") {
    data_pad <- data.frame(data, X, stringsAsFactors = FALSE)
    New_data_pad <- data.frame(New_data, New_X, stringsAsFactors = FALSE)
    names(data_pad) <- c(names(data), colnames(X))
    names(New_data_pad) <- c(names(New_data), colnames(New_X))

    pred <- paste("0 +", paste(colnames(X), collapse = " + "))
    fm <- as.formula(paste(names$y, "~", pred, "+ (", pred, "|", names$g, ")"))
    fit <- brokenstick_impl_lmer(data = data_pad,
                                 formula = fm,
                                 control = control,
                                 na.action = na.action)
  }

  obj <- new_brokenstick(call = call,
                         names = names,
                         internal = l$internal,
                         boundary = l$boundary,
                         degree = degree,
                         method = method,
                         control = control,
                         beta = fit$beta,
                         omega = fit$omega,
                         sigma2j = fit$sigma2j,
                         sigma2 = fit$sigma2,
                         light = light,
                         hide = hide,
                         data = data,
                         newdata = newdata,
                         sample = fit$sample,
                         imp = fit$imp,
                         mod = fit$mod)
  ## only the obj$mod need to be used in `brokenstick_impl_lmer`
  ## it is still the obe$mod == fit$mod for the
  return(obj)
}

### brokenstick_impl_lmer called in brokenstick_bridge -------------------------

brokenstick_impl_lmer <- function(data, formula, control, na.action) {

  # Bates et al, linear mixed-effects model
  mod <- lmer(formula = formula,
              data = data,
              control = control,
              na.action = na.action)

  ## Here we trust that names(slot(model, "cnms")) gives
  ## the name of the group variable
  df <- as.data.frame(VarCorr(mod))
  ## lmer is still the S4 object
  y <- mod@resp$y
  ## we only need this mod for the final model prediction
  ## we will keep it here for the integraty of the package.
  obj <- list(mod = mod,
              beta = fixef(mod),
              ## methods::slot() -----------------------------------------------
              ## The Slots in an S4 Object from a Formal Class Description
              ## These functions return or set information about
              ##  the individual slots in an object.
              ##  Note that the "@" operators for extraction
              ##  and replacement are primitive and actually
              ##  reside in the base package.
              omega = as.matrix(as.data.frame(VarCorr(mod)[[names(slot(mod, "cnms"))]])),
              sigma2j = numeric(),
              sigma2 = df[df$grp == "Residual", "vcov"],
              sample = c(length(y),
                         sum(!is.na(y)),
                         sum(is.na(y)),
                         as.integer(ngrps(mod)),
                         0L))
  ## only the obj$mod need to be used in the
  ## `brokenstick_impl_lmer()` function
  return(obj)
}

# Set control functions --------------------------------------------------------
## https://github.com/growthcharts/brokenstick/blob/master/R/set_control.R

#' Set controls to steer calculations
#'
#' @param method String indicating estimation method: `"kr"` or `"lmer"`
#' @param kr A list generated by [control_kr].
#' @param lmer A list generated by [lme4::lmerControl]. The default
#' is set to `lmerControl(check.nobs.vs.nRE = "warning")`, which turns
#' fatal errors with respect the number of parameters into warnings. Use
#' `lmerControl(check.nobs.vs.nRE = "ignore")` to silence `lmer()`.
#' @param \dots Forwards arguments to [control_kr()]
#' @return For method `"kr"`, a list returned by `control_kr()`.
#'         For method `"lmer"`, an object of class `lmerControl`.
#'         For other methods, `set_control()` returns `NULL`.`
#' @export
#'
set_control <- function(method = c("kr", "lmer"),
                        kr = control_kr(...),
                        lmer = lmerControl(check.nobs.vs.nRE = "warning"),
                        ...) {
  method <- match.arg(method)
  if (method == "kr") {
    return(kr)
  }
  if (method == "lmer") {
    return(lmer)
  }
  return(NULL)
}

### We do not concern this KR in the people-like-me methods --------------------
#' Set controls for Kasim-Raudenbush sampler
#' @param niter      Integer. Number of samples from posterior. Default:  `200`.
#' @param nimp      Integer. Number of multiple imputations. Default: `0`.
#' @param start    Integer. The iteration number of the first observation
#' @param thin     Integer. The thinning interval between consecutive observations
#' @param seed     Integer. Seed number for [base::set.seed()]. Use `NA` to
#' bypass seed setting.
#' @param cormodel String indicating the correlation model:
#'                 `"none"` (default), `"argyle"` or `"cole"`
#' @param \dots    Allow for dot parameters
#' @return A list with eight components. The function calculates parameters
#' `end` (the iteration number of the last iteration) and `thin_imp`
#' (thinning factor for multiple imputations) from the other inputs.
#' @export
control_kr <- function(niter = 200L,
                       nimp = 0L,
                       start = 101L,
                       thin = 1L,
                       seed = NA_integer_,
                       cormodel = c("none", "argyle", "cole"),
                       ...) {
  cormodel <- match.arg(cormodel)

  end <- start + niter * thin
  if (nimp > niter) {
    stop("Number of imputations (nimp = ", nimp, ") exceeds number of parameter draws (niter = ", niter, ").")
  }
  thin_imp <- ifelse(nimp, as.integer((end - start) / nimp), Inf)

  obj <- list(
    niter = niter,
    nimp = nimp,
    start = start,
    end = end,
    thin = thin,
    thin_imp = thin_imp,
    seed = seed,
    cormodel = cormodel
  )

  return(obj)
}


# Model.frame functions --------------------------------------------------------
#' @export
model.frame.brokenstick <- function(formula, data = NULL, ...) {
  if (formula$light) {
    return(NULL)
  }
  names <- unlist(formula$names)
  if (is.null(data)) data <- formula$data
  ff <- paste(names[["y"]], "~", names[["x"]], "+", names[["g"]])
  form <- as.formula(ff)
  return(model.frame.default(formula = form, data = data, ...))
}

#' @export
model.matrix.brokenstick <- function(object, ...) {
  if (object$light) {
    return(NULL)
  }
  x_name <- object$names$x[1L]
  obj <- make_basis(
    x = object$data[[x_name]],
    internal = get_knots(object, hide = "boundary"),
    boundary = get_knots(object, hide = "internal")
  )
  return(obj)
}


# Internal functions ----------------------------------------------------------
##
append_data <- function(data, names, x = NULL, y = NULL, group = NULL) {
  if (is.null(data)) stop("`data` not found.")
  if (is.null(x) && is.null(y) && is.null(group)) {
    return(data)
  }

  # Case 1: create x for every group in data
  if (!is.null(x) && is.null(y) && is.null(group)) {
    reset <- expand.grid(
      x = x,
      y = NA,
      g = unique(data[[names$g]])
    )
    colnames(reset) <- c(names$x, names$y, names$g)
    reset <- bind_rows(data = data, added = reset, .id = ".source")
    # message("Reset newdata: predict at `x` in every group.")
  }

  # Case 2: create new group with x and y
  if (!is.null(x) && !is.null(y) && is.null(group)) {
    if (length(x) != length(y)) {
      stop("Incompatible length of `x` and `y`.", call. = FALSE)
    }
    reset <- data.frame(
      s = "added",
      x = x,
      y = y,
      g = 0
    )
    colnames(reset) <- c(".source", names$x, names$y, names$g)
    # message("Reset newdata: new group with `x` and `y`.")
  }

  # Case 3: subset groups from data
  if (is.null(x) && is.null(y) && !is.null(group)) {
    if (!length(data)) {
      stop("A light brokenstick object expects a `newdata` argument.", call. = FALSE)
    }
    reset <- data[data[[names$g]] %in% group, , drop = FALSE]
    reset <- bind_cols(.source = "data", reset)
    # message("Reset newdata: subset of groups.")
  }

  # Case 4: create x for subset of groups from data
  if (!is.null(x) && is.null(y) && !is.null(group)) {
    groups <- intersect(data[[names$g]], group)
    reset <- expand.grid(
      s = "added",
      x = x,
      y = NA,
      g = groups
    )
    colnames(reset) <- c(".source", names$x, names$y, names$g)
    reset <- bind_rows(
      data = data[data[[names$g]] %in% groups, , drop = FALSE],
      added = reset, .id = ".source"
    ) %>%
      relocate(".source")
    # message("Reset newdata: predict at `x` in subset of groups.")
  }

  # Case 5: create data.frame from vectors x, y and group
  if (!is.null(x) && !is.null(y) && !is.null(group)) {
    if (length(x) != length(y)) {
      stop("Incompatible length of `x` and `y`.", call. = FALSE)
    }
    if (length(x) != length(group)) {
      stop("Incompatible length of `x` and `group`.", call. = FALSE)
    }
    groups <- intersect(data[[names$g]], group)
    reset <- data.frame(
      x = x,
      y = y,
      g = group
    )
    colnames(reset) <- c(names$x, names$y, names$g)
    reset <- bind_rows(
      data = data[data[[names$g]] %in% groups, , drop = FALSE],
      added = reset, .id = ".source"
    ) %>%
      relocate(".source")
    # message("Reset newdata: predict from vectors `x`, `y` and `group`.")
  }

  return(reset)
}

calculate_knots <- function(x, k, internal, boundary) {
  k_orig <- k
  knots_orig <- sort(internal)
  boundary_orig <- sort(boundary)

  # for NULL or length not 2, set boundary to data range
  if (length(boundary_orig) != 2L) {
    boundary <- range(x, na.rm = TRUE)
  }

  # make sure that boundary includes all knots
  if (!is.null(knots_orig)) {
    boundary[1L] <- min(min(knots_orig, na.rm = TRUE), boundary[1L])
    boundary[2L] <- max(max(knots_orig, na.rm = TRUE), boundary[2L])
  }

  if (length(knots_orig)) {
    # if there is vector input via knots
    # trim knots to exclude boundary points, and set k
    knots <- as.numeric(knots_orig)
    knots <- knots[knots > boundary[1L] & knots < boundary[2L]]
    k <- length(knots)
  } else {
    # no user knots, set knots from
    if (is.null(k_orig)) k_orig <- 0L
    if (k_orig >= 0L && k_orig <= 50L) {
      k <- k_orig
      knots <- quantile(x,
                        probs = seq(0, 1, length.out = k + 2L),
                        na.rm = TRUE
      )[-c(1L, k + 2L)]
    } else {
      stop("Number of internal knots `k` outside range 0-50")
    }
  }
  return(list(k = k, internal = knots, boundary = boundary))
}

install.on.demand <- function(pkg, quiet = FALSE, ...) {
  # internal function that checks whether package pkg is
  # in the library. If not found, it asks the user permission
  # to install from CRAN.
  if (requireNamespace(pkg, quietly = TRUE)) {
    return()
  }
  answer <- askYesNo(paste("Package", pkg, "needed. Install from CRAN?"))
  if (answer) install.packages(pkg, repos = "https://cloud.r-project.org/", quiet = quiet)
  return()
}


rho <- function(t1, t2, tau, lambda) {
  # Argyle et al 2008, eq 2 - Markov model
  return(((tau + t1) / (tau + t2))^lambda)
}

fn <- function(x, data) {
  tau <- x[1L]
  lambda <- x[2L]
  rhat <- with(data, rho(t1, t2, tau, lambda))
  return(sum((data$r - rhat)^2))
}

cor2cov <- function(cor, sd) {
  return(sweep(sweep(cor, 1L, sd, "*"), 2L, sd, "*"))
}

vec2cov <- function(vec, sd) {
  cov <- matrix(0, length(sd), length(sd))
  diag(cov) <- 0.5
  cov[lower.tri(cov)] <- vec
  cov <- cov + t(cov)
  return(cor2cov(cov, sd = sd))
}

cov2vec <- function(cov) {
  cor <- cov2cor(cov)
  return(list(vec = cor[lower.tri(cor)], sd = sqrt(diag(cov))))
}

smooth_covariance <- function(grid, cov, method = c("none", "argyle", "cole")) {
  if (method == "none") {
    return(cov)
  }
  d <- cov2vec(cov)
  grid$r <- d$vec

  # Argyle-Markov correlation model
  if (method == "argyle") {
    opt <- optim(c(1.3, 0.5), fn, data = grid)
    rhat <- with(grid, rho(t1, t2, tau = opt$par[1], lambda = opt$par[2]))
  }

  # Cole correlation model
  if (method == "cole") {
    grid$y <- log((1 + grid$r) / (1 - grid$r)) / 2
    fit <- lm(y ~ I(log((t1 + t2) / 2)) + I(log(t2 - t1)) + I(1 / (t2 - t1)) + I(log((t1 + t2) / 2) * log(t2 - t1)) + I(log((t1 + t2) / 2)^2),
              data = grid
    )
    yhat <- predict(fit)
    rhat <- (exp(2 * yhat) - 1) / (exp(2 * yhat) + 1)
  }

  return(vec2cov(rhat, sd = smooth(d$sd)))
}


#' Parse formula for brokenstick model
#'
#' A bare bones formula parser to extract variables names from
#' formulas of `y ~ x | g`. It return the name of
#' the first variable mentioned in each formula component.
#' @param f formula object
#' @return A `list` with elements `x`, `y` and `g`.
#' Each element has length 1.
#' @author Stef van Buuren 2023
parse_formula <- function(f) {
  stopifnot(inherits(f, "formula"))
  if (length(f[[3L]]) != 3L) {
    stop(call. = FALSE, "Can't find RHS expansion in formula.")
  }
  if (f[[3L]][[1L]] != "|") {
    stop(call. = FALSE, "Can't find `|` operator in formula.")
  }

  # Just take first variables - no support for `+` and friends
  y_name <- all.vars(f[[2L]], max.names = 1L)
  x_name <- all.vars(f[[3L]][[2L]], max.names = 1L)
  g_name <- all.vars(f[[3L]][[3L]], max.names = 1L)

  vec <- c(x_name, y_name, g_name)
  if (any(duplicated(vec))) {
    stop(call. = FALSE, "Found duplicate names in formula.")
  }
  if (any(vec == ".")) {
    stop(call. = FALSE, "No support for `.` in formula.")
  }

  return(list(x = x_name, y = y_name, g = g_name))
}

#' Create linear splines basis
#'
#' This function creates the basis function of a second-order (linear) splines
#' at a user-specific set of break points.
#' @aliases make_basis
#' @param x numeric vector
#' @param xname predictor name. Default is \code{"x"}
#' @param internal a vector of internal knots, excluding boundary knots
#' @param boundary vector of external knots
#' @param degree the degree of the spline. The broken stick model
#' requires linear splines, so the default is \code{degree = 1}.
#' Setting \code{degree = 0} yields (crisp) dummy coding, and one
#' column less than for \code{degree = 1}.
#' @param warn a logical indicating whether warnings from \code{splines::bs()}
#' should be given.
#' @return A matrix with \code{length(x)} rows and \code{length(breaks)}
#' columns, with some extra attributes described by \code{bs()}.
#' @author Stef van Buuren 2023
#' @note Before version 0.54, it was standard practice that the \code{knots}
#' array always included \code{boundary[1L]}.
make_basis <- function(x,
                       xname = "x",
                       internal = NULL,
                       boundary = range(x),
                       degree = 1L,
                       warn = FALSE) {

  # safety check: remove lower boundary knot from knots to be compatiable
  # with models fitted prior to version 0.53
  internal <- internal[internal > boundary[1L] & internal < boundary[2L]]

  # trick to evade error from bs() if x is fully NA
  padx <- all(is.na(x))
  if (padx) x <- c(0, x)

  # dummy coding if degree is zero
  if (degree == 0L) {
    df <- data.frame(x = cut(x,
                             breaks = c(boundary[1L], internal, boundary[2L]),
                             right = FALSE, include.lowest = TRUE
    ))
    X <- model.matrix(
      as.formula("~ 0 + x"),
      model.frame(~., df, na.action = na.pass)
    )
  }

  # fuzzy coding by linear spline
  if (degree >= 1L) {
    if (warn) {
      X <- splines::bs(
        x = x,
        knots = c(boundary[1L], internal),
        Boundary.knots = boundary,
        degree = degree
      )
    } else {
      suppressWarnings({
        X <- splines::bs(
          x = x,
          knots = c(boundary[1L], internal),
          Boundary.knots = boundary,
          degree = degree
        )
      })
    }
  }

  knots <- sort(unique(c(boundary, internal)))
  if (degree == 0L) knots <- knots[-length(knots)]
  colnames(X) <- paste(xname, as.character(knots), sep = "_")

  # restore original if padded
  if (padx) X <- X[-1L, , drop = FALSE]

  return(X)
}



