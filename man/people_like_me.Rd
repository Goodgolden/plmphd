% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_people-like-us.R
\name{people_like_me}
\alias{people_like_me}
\title{People-Like-Me Methods for Single Testing Individual}
\usage{
people_like_me(
  train_data,
  test_data,
  outcome_var,
  time_var,
  id_var,
  tmin,
  tmax,
  brokenstick_knots,
  anchor_time,
  linear_formula,
  gamlss_formula,
  gamlss_sigma,
  match_methods = c("euclidean", "mahalanobis", "single"),
  weight = FALSE,
  match_alpha = NULL,
  match_number = NULL,
  match_plot = TRUE,
  predict_plot = TRUE,
  ...
)
}
\arguments{
\item{train_data}{\code{data.frame} The training dataset for fitting the brokenstick and linear models. This dataset also serves as the pool for the matching process.}

\item{test_data}{\code{data.frame} The testing dataset for personalized prediction.}

\item{outcome_var}{\code{character} The name of the outcome variable of interest.}

\item{time_var}{\code{character} The name of the time variable of interest.}

\item{id_var}{\code{character} The name of the ID variable representing individuals.}

\item{tmin}{\code{numeric} The minimum time point for the prediction.}

\item{tmax}{\code{numeric} The maximum time point for the prediction.}

\item{brokenstick_knots}{\code{numeric} A vector of knots for the brokenstick model, which do not need to be equally spaced.}

\item{anchor_time}{\code{numeric} A time point (or multiple time points) used for the anchor in the matching process.}

\item{linear_formula}{\code{formula} The formula used to fit the linear model.}

\item{gamlss_formula}{\code{formula} The mean formula for the GAMLSS model, typically used for smoothing as a function of time.}

\item{gamlss_sigma}{\code{formula} The sigma formula for the GAMLSS model, used for smoothing as a function of time.}

\item{match_methods}{\code{character} Distance methods used for matching. Options are \code{"euclidean"}, \code{"mahalanobis"}, or \code{"single"}.}

\item{weight}{\code{logical} Whether to apply weights in the GAMLSS prediction process. Defaults to \code{FALSE}.}

\item{match_alpha}{\code{numeric} Alpha parameter for the matching process, typically used in Mahalanobis distance. Can be \code{NULL}.}

\item{match_number}{\code{numeric} The number of matches to find during the matching process. Can be \code{NULL} or a numeric value.}

\item{match_plot}{\code{logical} If \code{TRUE}, a plot of the matching process will be generated. Defaults to \code{TRUE}.}

\item{predict_plot}{\code{logical} If \code{TRUE}, a plot of the prediction process will be generated. Defaults to \code{TRUE}.}

\item{...}{Other arguments passed to the function.}
}
\value{
A \code{list} containing the following elements:
\itemize{
\item \code{predicted_centiles}: A \code{data.frame} of predicted centiles for the individual in the test dataset.
\item \code{observed_centiles}: A \code{data.frame} of observed centiles for the individual in the test dataset.
\item \code{matches}: A plot of the matching process, if \code{match_plot = TRUE}.
\item \code{plot}: A plot of the predicted centiles, if \code{predict_plot = TRUE}.
\item \code{gamlss_data}: A \code{data.frame} of the subset used for the GAMLSS prediction.
}
}
\description{
This function is part of the People-Like-Me method and is designed for personalized prediction of centiles for a single individual in the testing dataset. The function fits a brokenstick and linear model to the training data, and then uses the models to make predictions for the testing individual. Matching is performed based on multiple anchor time points using various distance metrics such as Euclidean and Mahalanobis distance. The function returns predicted centiles using the GAMLSS model, and includes the option to visualize the matching and prediction process with plots.
}
\examples{
\dontrun{
  # Example usage
  results <- people_like_me(train_data, test_data, outcome_var = "height", time_var = "age", id_var = "id", ...)
}

}
\seealso{
\code{\link{people_like_i}}, \code{\link{people_like_us}}, \code{\link{people_like_thee}}
}
