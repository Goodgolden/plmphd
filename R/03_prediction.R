## 3.1 people like me prediction -----------------------------------------------
#' Prediction with GAMLSS Model
#'
#' @description This function performs centile prediction using the GAMLSS model for a target individual. It fits the GAMLSS model to the matching dataset and predicts the centiles for the test individual. The function can optionally generate a plot comparing the predicted centiles with the individual’s observational data.
#'
#' @param matching \code{data.frame} The dataset of matched individuals from the original dataset.
#' @param test_one \code{data.frame} The dataset for the target individual for whom predictions are made.
#' @param id_var \code{character} The name of the ID variable in the dataset.
#' @param time_var \code{character} The name of the time variable in the dataset.
#' @param outcome_var \code{character} The name of the outcome variable in the dataset.
#' @param tmin \code{numeric} The minimum time point for the prediction.
#' @param tmax \code{numeric} The maximum time point for the prediction.
#' @param gamlss_formula \code{formula} The formula for the mean function of the GAMLSS model.
#' @param gamsigma_formula \code{formula} The formula for the sigma (scale) function of the GAMLSS model.
#' @param weight \code{logical} Indicates whether to use weighted regression for the GAMLSS model based on matching. Defaults to \code{FALSE}.
#' @param predict_plot \code{logical} Whether to include a plot of the predictions. Defaults to \code{TRUE}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{centiles_observed}: A \code{data.frame} of observed centiles for the individual, along with coverage and bias.
#'   \item \code{centiles_predicted}: A \code{data.frame} of predicted centiles (5th, 10th, 25th, 50th, 75th, 90th, 95th percentiles) over time.
#'   \item \code{predictive_centiles}: A \code{ggplot} object of the centile prediction plot if \code{predict_plot = TRUE}, otherwise \code{NULL}.
#' }
#'
#' @seealso \code{\link{plm_ind_plot}} for the plotting function used.
#'
#' @examples
#' \dontrun{
#'   result <- predict_gamlss(matching = matched_data,
#'                            test_one = individual_data,
#'                            id_var = "id",
#'                            time_var = "time",
#'                            outcome_var = "height",
#'                            tmin = 1, tmax = 10,
#'                            gamlss_formula = "height ~ time",
#'                            gamsigma_formula = "~ time",
#'                            weight = FALSE,
#'                            predict_plot = TRUE)
#' }
#'
#' @export
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
#' Plot Individual People-Like-Me Matching
#'
#' @description This function generates a plot for an individual's predicted quantiles over time, along with the observational data from the original dataset. It visualizes the range of predictions using quantile ribbons and lines, and overlays the individual's actual observations.
#'
#' @param quantile \code{data.frame} A dataset containing the predicted quantiles (e.g., q05, q10, q25, q50, q75, q90, q95) for the individual over time.
#' @param observation \code{data.frame} The observational data for the individual from the original dataset.
#' @param title \code{character} The title for the plot. Defaults to \code{NULL}.
#' @param outcome_var \code{character} The name of the outcome variable in the dataset.
#' @param id_var \code{character} The name of the ID variable in the dataset.
#' @param time_var \code{character} The name of the time variable in the dataset.
#' @param ... Other arguments passed to the function (optional).
#'
#' @return A \code{ggplot} object representing the plot with the individual's observational data and predicted quantiles.
#'
#' @details The plot includes multiple quantile bands (5th-95th, 10th-90th, 25th-75th percentiles) represented by shaded ribbons, with dashed lines for key quantiles. The individual's observed data points are plotted on top of the quantile bands for easy comparison.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   plm_ind_plot(quantile = pred_quantiles,
#'                observation = obs_data,
#'                title = "Individual Prediction and Observations",
#'                outcome_var = "height",
#'                id_var = "id",
#'                time_var = "age")
#' }

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



