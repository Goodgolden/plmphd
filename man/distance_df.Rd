% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_distance.R
\name{distance_df}
\alias{distance_df}
\title{Distance Calculation for Matching}
\usage{
distance_df(
  lb_train,
  lb_test_ind,
  match_methods = c("euclidean", "mahalanobis", "single"),
  match_time = NULL,
  id_var,
  outcome_var,
  time_var,
  ...
)
}
\arguments{
\item{lb_train}{\code{data.frame} The training dataset containing predictions or imputations from two-stage models (brokenstick model and linear model).}

\item{lb_test_ind}{\code{data.frame} The test individual’s data containing predictions or imputations from the two-stage models.}

\item{match_methods}{\code{character} The method for distance calculation. Options are \code{"euclidean"}, \code{"mahalanobis"}, or \code{"single"} for single-time matching.}

\item{match_time}{\code{numeric} The critical time point for single-time matching. This time must be included in the anchor time set. Not required for \code{"euclidean"} or \code{"mahalanobis"} methods.}

\item{id_var}{\code{character} The name of the ID variable in the dataset.}

\item{outcome_var}{\code{character} The name of the outcome variable in the dataset.}

\item{time_var}{\code{character} The name of the time variable in the dataset.}

\item{...}{Additional arguments passed to the function.}
}
\value{
A \code{data.frame} containing the calculated distances and, for Mahalanobis distance, p-values, arranged from smallest to largest.
}
\description{
This function calculates the distance between the training data and a test individual based on different matching methods, such as Euclidean distance, Mahalanobis distance, or single-time point matching. The function returns a distance dataframe with distances and optionally p-values, arranged from smallest to largest.
}
\examples{
\dontrun{
  distance_df(lb_train = train_data,
              lb_test_ind = test_individual_data,
              match_methods = "euclidean",
              id_var = "id",
              outcome_var = "height",
              time_var = "time")
}
}
