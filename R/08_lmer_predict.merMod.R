function (object, newdata = NULL, newparams = NULL, re.form = NULL,
          ReForm, REForm, REform, random.only = FALSE, terms = NULL,
          type = c("link", "response"), allow.new.levels = FALSE, na.action = na.pass,
          se.fit = FALSE, ...)
{
  re.form <- reFormHack(re.form, ReForm, REForm, REform)
  if (...length() > 0)
    warning("unused arguments ignored")
  type <- match.arg(type)
  if (!is.null(terms))
    stop("terms functionality for predict not yet implemented")
  if (!is.null(newparams))
    object <- setParams(object, newparams)
  if (is.null(newdata) && is.null(re.form) && is.null(newparams) &&
      !random.only) {
    if (isLMM(object) || isNLMM(object)) {
      pred <- na.omit(fitted(object))
    }
    else {
      pred <- switch(type, response = object@resp$mu, link = object@resp$eta)
      if (is.null(nm <- rownames(model.frame(object))))
        nm <- seq_along(pred)
      names(pred) <- nm
    }
    fit.na.action <- NULL
  }
  else {
    fit.na.action <- attr(object@frame, "na.action")
    nobs <- if (is.null(newdata))
      nrow(object@frame)
    else nrow(newdata)
    pred <- rep(0, nobs)
    if (!random.only) {
      X <- getME(object, "X")
      X.col.dropped <- attr(X, "col.dropped")
      if (is.null(newdata)) {
        offset <- model.offset(model.frame(object))
        if (is.null(offset))
          offset <- 0
      }
      else {
        ## Here is the start of using newdata
        RHS <- formula(substitute(~R, list(R = RHSForm(formula(object,
                                                               fixed.only = TRUE)))))
        orig.fixed.levs <- get.orig.levs(object, fixed.only = TRUE,
                                         newdata = newdata)
        mfnew <- suppressWarnings(model.frame(delete.response(terms(object,
                                                                    fixed.only = TRUE, data = newdata)), newdata,
                                              na.action = na.action, xlev = orig.fixed.levs))
        X <- model.matrix(RHS, data = mfnew, contrasts.arg = attr(X,
                                                                  "contrasts"))
        offset <- 0
        tt <- terms(object, data = newdata)
        if (!is.null(off.num <- attr(tt, "offset"))) {
          for (i in off.num) offset <- offset + eval(attr(tt,
                                                          "variables")[[i + 1]], newdata)
        }
        fit.na.action <- attr(mfnew, "na.action")
        if (is.numeric(X.col.dropped) && length(X.col.dropped) >
            0)
          X <- X[, -X.col.dropped, drop = FALSE]
      }
      pred <- drop(X %*% fixef(object))
      pred <- pred + offset
    }
    if (isRE(re.form)) {
      if (is.null(re.form))
        re.form <- reOnly(formula(object))
      rfd <- if (is.null(newdata)) {
        tryCatch(getData(object), error = function(e) object@frame)
      }
      else newdata
      newRE <- mkNewReTrms(object, rfd, re.form, na.action = na.action,
                           allow.new.levels = allow.new.levels)
      REvals <- base::drop(as(newRE$b %*% newRE$Zt, "matrix"))
      if (class(fit.na.action) %in% c("omit", "exclude") &&
          length(fit.na.action) > 0) {
        REvals <- REvals[-fit.na.action]
      }
      pred <- pred + REvals
      if (random.only) {
        fit.na.action <- attr(newRE$Zt, "na.action")
      }
    }
    if (isGLMM(object) && type == "response") {
      pred <- object@resp$family$linkinv(pred)
    }
  }
  if (is.null(newdata)) {
    fit.na.action <- attr(model.frame(object), "na.action")
    if (!missing(na.action)) {
      if (!is.null(fit.na.action))
        class(fit.na.action) <- class(attr(na.action(NA),
                                           "na.action"))
    }
  }
  pred <- napredict(fit.na.action, pred)
  if (!se.fit)
    return(pred)
  if (!isLMM(object))
    warning("se.fit computation uses an approximation to estimate the sampling distribution of the parameters")
  s <- sigma(object)
  L <- getME(object, "L")
  RX <- getME(object, "RX")
  RZX <- getME(object, "RZX")
  Lambdat <- getME(object, "Lambdat")
  RXtinv <- solve(t(RX))
  LinvLambdat <- solve(L, Lambdat, system = "L")
  Minv <- s * rbind(cbind(LinvLambdat, Matrix(0, nrow = nrow(L),
                                              ncol = ncol(RX))), cbind(-RXtinv %*% t(RZX) %*% LinvLambdat,
                                                                       RXtinv))
  Cmat <- crossprod(Minv)
  if (is.null(newdata)) {
    X <- getME(object, "X")
    if (is.null(re.form)) {
      Z <- getME(object, "Z")
    }
    else {
      if (isRE(re.form)) {
        Z <- t(newRE$Zt)
      }
      else {
        Z <- Matrix(0, nrow = nrow(X), ncol = ncol(L))
      }
    }
  }
  else {
    if (isRE(re.form)) {
      Z <- t(newRE$Zt)
    }
    else {
      Z <- Matrix(0, nrow = nrow(X), ncol = ncol(L))
    }
  }
  if (random.only)
    X <- Matrix(0, nrow = nrow(Z), ncol = nrow(RX))
  ZX <- cbind(Z, X)
  list(fit = pred, se.fit = sqrt(quad.tdiag(Cmat, ZX)))
}
