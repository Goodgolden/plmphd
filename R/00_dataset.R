#' Example Dataset: Simulated EPIC Children's Height Growth Based on the Preece-Baines I Model
#'
#' This dataset is an example for the EPIC children's height growth study. It includes
#' 500 individuals in the training set and 10 individuals in the testing set.
#' The variables are described below:
#'
#' @format A training tibble with 9,363 rows and 5 variables, and
#'         a testing tibble with 186 rows and 5 variables:
#' \describe{
#'   \item{id}{factor}{Unique identifier for each individual.}
#'   \item{time}{numeric}{Time of measurement, in appropriate time units.}
#'   \item{sex}{factor}{Gender of the individual.}
#'   \item{ht}{numeric}{Height of the individual, in appropriate measurement units.}
#'   \item{group}{factor}{Indicates whether the observation belongs to the training or testing group.}
#' }
#'
#' @source \url{https://github.com/yourusername/yourrepo}
"train"
"test"
