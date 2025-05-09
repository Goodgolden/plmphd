% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_distance.R
\name{dis_match}
\alias{dis_match}
\title{Distance Calculation and Matching for People-Like-Me}
\usage{
dis_match(
  lb_train,
  lb_test_ind,
  train = train,
  match_methods = c("euclidean", "mahalanobis", "single"),
  id_var,
  outcome_var,
  time_var,
  match_alpha = NULL,
  match_number = NULL,
  match_time = NULL,
  match_plot,
  ...
)
}
\arguments{
\item{lb_train}{\code{data.frame} The training dataset containing predictions or imputations from the two-stage models (brokenstick and linear models).}

\item{lb_test_ind}{\code{data.frame} The testing individual’s dataset containing predictions or imputations from the two-stage models.}

\item{train}{\code{data.frame} The original training dataset used to extract the matched individuals.}

\item{match_methods}{\code{character} The distance calculation method. Options are \code{"euclidean"}, \code{"mahalanobis"}, or \code{"single"} (for single-time matching).}

\item{id_var}{\code{character} The name of the ID variable in the dataset.}

\item{outcome_var}{\code{character} The name of the outcome variable in the dataset.}

\item{time_var}{\code{character} The name of the time variable in the dataset.}

\item{match_alpha}{\code{numeric} The p-value threshold for Mahalanobis distance matching. If \code{NULL}, it is ignored.}

\item{match_number}{\code{numeric} The number of matches to retrieve from the distance dataframe for Euclidean or Mahalanobis distance matching. If \code{NULL}, it is ignored.}

\item{match_time}{\code{numeric} The critical time point for single-time matching. This time must be included in the anchor time set. Not required for Euclidean and Mahalanobis methods.}

\item{match_plot}{\code{logical} Whether to generate a plot of the matching individuals' trajectories. Defaults to \code{FALSE}.}

\item{...}{Additional arguments passed to the function.}
}
\value{
A list containing:
\itemize{
\item \code{subset}: A \code{data.frame} of the matched subset from the training dataset based on the chosen matching method.
\item \code{target}: The \code{data.frame} of the test individual data.
\item \code{plot}: A \code{ggplot} object showing the matching individual trajectories compared to the test individual if \code{match_plot = TRUE}, otherwise \code{NULL}.
}
}
\description{
This function calculates distances between a test individual and the training dataset based on the specified distance method (Euclidean, Mahalanobis, or single-time matching). It then returns a subset of matching individuals from the training dataset based on the calculated distances or p-values and can optionally generate a plot of the matching trajectories.
}
\examples{
\dontrun{
  matches <- dis_match(lb_train = train_pred,
                       lb_test_ind = test_pred,
                       train = train_data,
                       match_methods = "euclidean",
                       id_var = "id",
                       outcome_var = "height",
                       time_var = "age",
                       match_number = 5,
                       match_plot = TRUE)
}
}
