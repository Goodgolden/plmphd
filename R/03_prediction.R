## 3.1 people like me prediction -----------------------------------------------
#' Title Prediction with GAMLSS model
#'
#' @param matching
#' @param test_one
#' @param id_var
#' @param time_var
#' @param outcome_var
#' @param gamlss_formula
#' @param gamsigma_formula
#' @param weight
#' @param predict_plot
#'
#' @return
#' @export
#'
#' @examples
predict_gamlss <- function(matching,
                           test_one,
                           id_var,
                           time_var,
                           outcome_var,
                           tmin,
                           tmax,
                           gamlss_formula,
                           gamsigma_formula,
                           weight = FALSE,
                           predict_plot) {


  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)

  ## Here is the current setup for the weights
  ## There is a problem when we use the weights
  if (weight == FALSE) {
    w = NULL
  } else {
    w = matching$pvalue
  }

  if (is.null(matching$pvalue)) {
    matching <<- matching %>% dplyr::select(-diff)
  } else {
    matching <<- matching %>% dplyr::select(-diff, -pvalue)
    # test_one[[as.character({{ time_var }})]]
  }

  plm <- gamlss::gamlss(as.formula(gamlss_formula),
                        sigma.formula = as.formula(gamsigma_formula),
                        # nu.formula = ~cs(time^0.1, df=1),
                        # tau.formula = ~cs(time^0.5, df=1),
                        # weights = w,
                        method = RS(100),
                        trace = FALSE,
                        data = matching,
                        family = NO)


  centiles_obs <-  gamlss::centiles.pred(plm, type = c("centiles"),
                                         xname = as.character({{time_var}}),
                                         xvalues = test_one$time ,
                                         cen = c(5, 10, 25, 50, 75, 90, 95)) %>%
    cbind(actual = test_one[[as.character({{ outcome_var }})]]) %>%
    as.data.frame() %>%
    mutate(coverage50 = ifelse(actual >= `25` & actual <= `75`, 1, 0),
           coverage80 = ifelse(actual >= `10` & actual <= `90`, 1, 0),
           coverage90 = ifelse(actual >= `5` & actual <= `95`, 1, 0),
           # mse = (actual - `50`)^2,
           # biassq = bias^2,
           # var = mse - bias^2,
           bias = abs(actual - `50`))

  centiles_pred <-
    centiles.pred(plm,
                  linetype = c("centiles"),
                  xname = "time",
                  xvalues = c(tmin:tmax),
                  cent = c(5, 10, 25, 50, 75, 90, 95),
                  plot = FALSE,
                  legend = T) %>%
    dplyr::select(time = 1,
                  q05 = 2,
                  q10 = 3,
                  q25 = 4,
                  q50 = 5,
                  q75 = 6,
                  q90 = 7,
                  q95 = 8) %>%
    mutate(cfint90 = q95 - q05,
           cfint80 = q90 - q10,
           cfint50 = q75 - q25)

  # cat("\n gamlss model prediction is done \n")

  if (predict_plot == TRUE) {
    plm_plot <- plm_ind_plot(quantile = centiles_pred,
                             observation = test_one,
                             outcome_var = outcome_var,
                             id_var = id_var,
                             time_var = time_var)
  } else {
    plm_plot <- NULL
  }

  return(list(centiles_observed = centiles_obs,
              centiles_predicted = centiles_pred,
              predictive_centiles = plm_plot))
}

## 3.2 individual people-like-me matching plot ---------------------------------
#' Title plot individual matching
#'
#' @param quantile
#' @param observation
#' @param title
#'
#' @return
#' @export
plm_ind_plot <- function(quantile,
                         observation,
                         title = NULL,
                         outcome_var,
                         id_var,
                         time_var,
                         ...) {
  observation <- observation %>%
    mutate(time = !!time_var,
                  outcome = !!outcome_var)

  plot1 <- ggplot() +
    geom_line(data = quantile, aes(x = time, y = q05),
              color = "dodgerblue", linetype = "dashed",
              alpha = 0.5) +
    geom_line(data = quantile, aes(x = time, y = q95),
              color = "dodgerblue", linetype = "dashed",
              alpha = 0.5) +
    geom_ribbon(data = quantile,
                aes(x = time, ymin = q05, ymax = q95),
                fill = "dodgerblue", alpha = 0.5) +
    geom_line(data = quantile, aes(x = time, y = q10),
              color = "dodgerblue2", linetype = "dashed",
              alpha = 0.7) +
    geom_line(data = quantile, aes(x = time, y = q90),
              color = "dodgerblue2", linetype = "dashed",
              alpha = 0.7) +
    geom_ribbon(data = quantile,
                aes(x = time, ymin = q10, ymax = q90),
                fill = "dodgerblue2", alpha = 0.7) +
    geom_line(data = quantile, aes(x = time, y = q25),
              color = "dodgerblue3", linetype = "dashed",
              alpha = 0.8) +
    geom_line(data = quantile, aes(x = time, y = q75),
              color = "dodgerblue3", linetype = "dashed",
              alpha = 0.8) +
    geom_ribbon(data = quantile,
                aes(x = time, ymin = q25, ymax = q75),
                fill = "dodgerblue3", alpha = 0.8) +
    geom_line(data = quantile, aes(x = time, y = q50),
              color = "dodgerblue4", linetype = "dashed") +
    geom_point(data = observation, 
               aes(x = time, 
                   y = outcome),
               color = "black", 
               size = 1) +
    theme_bw() +
    xlab("Time") +
    ylab("Outcome") +
    ggtitle(title)

  # print(range(observation$time))
  plot1
}



