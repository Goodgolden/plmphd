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
  browser()
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
