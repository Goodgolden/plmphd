# Miscellaneous function -------------------------------------------------------

## this file contains some of the functions used in this project
## be careful use in other situations, not sure for correctness

## read_excel_allsheets --------------------------------------------------------
#' Read Multiple Sheets from an Excel File
#'
#' @description This function reads all sheets from an Excel file and returns them as a list of data frames or tibbles, depending on the format specified by the \code{tibble} argument.
#'
#' @param filename \code{character} The file path of the Excel file to read.
#' @param tibble \code{logical} If \code{TRUE}, the sheets will be returned as tibbles (default behavior of \code{readxl}). If \code{FALSE}, the sheets will be converted to \code{data.frame}. Defaults to \code{FALSE}.
#'
#' @return A list of data frames (or tibbles), where each element corresponds to a sheet in the Excel file, with sheet names as the list names.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Reading an Excel file and returning the sheets as data.frames
#'   sheets <- read_excel_allsheets("path_to_file.xlsx", tibble = FALSE)
#'
#'   # Reading an Excel file and returning the sheets as tibbles
#'   sheets <- read_excel_allsheets("path_to_file.xlsx", tibble = TRUE)
#' }

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  return(x)
}

## letters_only -----------------------------------------------------------------
#' Check if a String Contains Only Letters
#'
#' @description This function checks whether the input string contains only alphabetic characters (A-Z, a-z), returning \code{TRUE} if the string contains only letters and \code{FALSE} otherwise.
#'
#' @param x \code{character} The input string to be checked.
#'
#' @return \code{logical} \code{TRUE} if the string contains only letters, otherwise \code{FALSE}.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Check if a string contains only letters
#'   letters_only("HelloWorld")  # TRUE
#'   letters_only("Hello123")    # FALSE
#' }



## numbers_only ----------------------------------------------------------------
#' Check if a String Contains Only Numbers
#'
#' @description This function checks whether the input string contains only numeric characters (0-9), returning \code{TRUE} if the string contains only numbers and \code{FALSE} otherwise.
#'
#' @param x \code{character} The input string to be checked.
#'
#' @return \code{logical} \code{TRUE} if the string contains only numeric characters, otherwise \code{FALSE}.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Check if a string contains only numbers
#'   numbers_only("123456")   # TRUE
#'   numbers_only("123abc")   # FALSE
#' }


## package info ----------------------------------------------------------------
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to ", pkgname, " version 0.1.2 !\n",
                        "For help, use: ?plmphd\n",
                        "To cite this package in publications, use: citation('", pkgname, "')\n",
                        "\n",
                        "Note: This package makes use of the following important packages:\n",
                        " - brokenstick: To cite brokenstick, use: citation('brokenstick')\n",
                        " - JMbayes2: To cite JMbayes2, use: citation('JMbayes2')\n",
                        "\n",
                        "Check the documentation for more details and examples! \n",
                        "This package is developed for Randy Jin's PhD thesis and Scientific Reports paper. \n")
}


.onLoad <- function(libname, pkgname) {
  ## Retrieve the current R options
  op <- options()

  ## Define custom options related
  ## to package development using devtools
  op.devtools <- list(
    devtools.path = "~/R-dev",
    ## Default path for package development
    devtools.install.args = "",
    ## Arguments to be passed to install.packages
    devtools.name = "Randy Jin",
    devtools.desc.author = c("Randy Jin <xin.2.jin@cuanschutz.edu> [aut, cre]",
                             "Elizabeth Juarez-Colunga <elizabeth.juarez-colunga@cuanschutz.edu> [ctb]",
                             "Carsten Goerg <carsten.Goerg@cuanschutz.edu> [ctb]"),
    ## Package author and maintainer
    devtools.desc.license = "MIT",
    ## Default license for the package
    devtools.desc.suggests = NULL,
    ## Suggested packages, if any
    devtools.desc = list()
    ## Additional description fields can be added here
  )

  ## Check if the options are already set,
  ## and set them only if they are not
  toset <- !(names(op.devtools) %in% names(op))
  if (any(toset)) options(op.devtools[toset])

  ## Ensure the function runs silently
  invisible()
}


## 0.1 norm L2 {{{--------------------------------------------------------------
#' Title L2 or other norms
#'
#' @param v any numeric vector
#' @return the L2 norm of the vector
norm2 <- function(v) {
  sqrt(sum(v^2))
}

## Add other type of norms???

## 0.2 not all na {{{-----------------------------------------------------------
#' Check if Not All Values Are Missing (NA)
#'
#' @description This function checks if a dataset (or vector) contains at least one non-missing (non-NA) value. It returns \code{TRUE} if there is at least one non-NA value, and \code{FALSE} if all values are missing (NA).
#'
#' @param x The target dataset (can be a vector, data frame, or matrix).
#'
#' @return \code{logical} A boolean result: \code{TRUE} if at least one value is not missing (NA) in the dataset, otherwise \code{FALSE}.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Check if a vector has at least one non-NA value
#'   not_all_na(c(NA, NA, 3))   # TRUE
#'   not_all_na(c(NA, NA, NA))  # FALSE
#'
#'   # Check if a data frame has at least one non-NA value
#'   df <- data.frame(a = c(NA, NA, NA), b = c(NA, 2, NA))
#'   not_all_na(df)  # TRUE
#' }
not_all_na <- function(x) {
  any(!is.na(x))
}


## 0.3 not any na {{{-----------------------------------------------------------
#' Check if There Are No Missing Values (NA) in a Dataset
#'
#' @description This function checks if a dataset (or vector) contains no missing values (NA). It returns \code{TRUE} if there are no NA values, and \code{FALSE} otherwise.
#'
#' @param x The target dataset (can be a vector, data frame, or matrix).
#'
#' @return \code{logical} A boolean result: \code{TRUE} if there are no missing values (NA) in the dataset, otherwise \code{FALSE}.
#'
#' @examples
#' \dontrun{
#'   # Check if a vector contains no missing values
#'   not_any_na(c(1, 2, 3, NA))  # FALSE
#'   not_any_na(c(1, 2, 3, 4))   # TRUE
#'
#'   # Check if a data frame contains no missing values
#'   df <- data.frame(a = c(1, 2, NA), b = c(4, 5, 6))
#'   not_any_na(df)  # FALSE
#' }
not_any_na <- function(x) {
  all(!is.na(x))
}



## 0.4 not in {{{---------------------------------------------------------------
`%!in%` <- Negate(`%in%`)


# Distance ---------------------------------------------------------------------

## 0.5 euclidean_df ----------------------------------------------------------
#' Calculate Euclidean Distance for a Matrix
#'
#' @description This function calculates the Euclidean distance between each column in a given numeric data matrix and a specified center. The result is returned as a list of distances, ordered from smallest to largest difference.
#'
#' @param Dmatrix \code{matrix} A numeric data matrix where each column represents a data point.
#' @param center \code{numeric} A numeric vector representing the center point from which the Euclidean distances are calculated.
#'
#' @return A \code{data.frame} with two columns: \code{id}, which represents the column names of the data matrix, and \code{diff}, which contains the Euclidean distances, ordered from smallest to largest.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example usage
#'   Dmatrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#'   center <- c(1, 2)
#'   euclidean_df(Dmatrix, center)
#' }
euclidean_df <- function(Dmatrix,
                         center) {
  matching <<- as.data.frame(Dmatrix - center) %>%
    apply(2, norm, type = "2") %>%
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    arrange(diff)

  return(matching)
}


## 0.6 mahalanobis_df --------------------------------------------------------
#' Calculate Mahalanobis Distance for a Matrix
#'
#' @description This function calculates the Mahalanobis distance between each column of a given numeric data matrix and a specified center. The result is returned as a list of distances, arranged from smallest to largest based on p-values.
#'
#' @param Dmatrix \code{matrix} A numeric data matrix where each column represents a data point.
#' @param center \code{numeric} A numeric vector representing the center point from which the Mahalanobis distances are calculated.
#'
#' @return A \code{data.frame} with two columns: \code{id}, which represents the column names of the data matrix, and \code{diff}, which contains the Mahalanobis distances. The rows are ordered by descending p-values.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example usage
#'   Dmatrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#'   center <- c(1, 2)
#'   mahalanobis_df(Dmatrix, center)
#' }
mahalanobis_df <- function(Dmatrix,
                           center) {

  def <- nrow(Dmatrix)
  df <- Dmatrix %>%
    as.matrix() %>%
    t()

  x <- sweep(df, 2L, center)
  invcov <- MASS::ginv(cov(df))

  value <- rowSums(x %*% invcov * x)
  pvalue <- pchisq(value, df = def, lower.tail = FALSE)
  matching <<- data.frame(diff = value,
                          pvalue = pvalue) %>%
    arrange(desc(pvalue)) %>%
    rownames_to_column("id")

  return(matching)
}



## 0.7 single_df {{{---------------------------------------------------------
#' Single Time Point Matching
#'
#' @description This function calculates the distance at a single time point from a given distance matrix. It returns a list of distances, ordered from smallest to largest based on the absolute difference from a specified center.
#'
#' @param Dmatrix \code{matrix} A numeric distance matrix where rows represent time points.
#' @param match_time \code{numeric} The anchor time point for matching. This specifies which row (time point) to use for distance calculation.
#' @param center \code{numeric} The value representing the center (reference point) from which distances are calculated.
#'
#' @return A \code{data.frame} with two columns: \code{id}, which represents the identifiers for each element, and \code{diff}, which contains the calculated distances, ordered from smallest to largest absolute difference from the center.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example usage
#'   Dmatrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
#'   match_time <- 2
#'   center <- 3
#'   single_df(Dmatrix, match_time, center)
#' }
single_df <- function(Dmatrix,
                      match_time,
                      center) {
  matching <<- as.data.frame(Dmatrix) %>%
    filter(as.numeric(rownames(.)) == match_time) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    mutate(diff = diff - as.numeric(center)) %>%
    rownames_to_column("id") %>%
    arrange(abs(diff))

  return(matching)
}

## 0.8 singletime_n {{{---------------------------------------------------------
#' Single Time Point Matching with a Specified Number of Matches
#'
#' @description This function calculates the distance at a single time point from a given distance matrix, and returns the top \code{match_num} closest matches based on the absolute difference from the time point.
#'
#' @param Dmatrix \code{matrix} A numeric distance matrix where rows represent time points.
#' @param match_time \code{numeric} The anchor time point for matching. This specifies which row (time point) to use for distance calculation.
#' @param match_num \code{numeric} The number of closest matches to return.
#'
#' @return A \code{data.frame} with two columns: \code{id}, representing the identifiers for each element, and \code{diff}, which contains the calculated distances, ordered from smallest to largest. The result is limited to the top \code{match_num} closest matches.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example usage
#'   Dmatrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
#'   match_time <- 2
#'   match_num <- 2
#'   singletime_n(Dmatrix, match_time, match_num)
#' }
singletime_n <- function(Dmatrix,
                         match_time,
                         match_num) {
  matching <<- Dmatrix %>%
    filter(as.numeric(rownames(.)) == match_time) %>%
    t() %>%
    ## using Frobenius norm
    # apply(lb_sub, 2, norm, type = "f") %>%
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    arrange(abs(diff)) %>%
    slice(1:match_num)

  return(matching)
}


## 0.9 euclidean_n -----------------
#' Euclidean Distance Matching with \eqn{\kappa} Criteria
#'
#' @description This function calculates the Euclidean distance for each column in a given distance matrix and returns the top \code{match_num} closest matches based on the smallest Euclidean distances.
#'
#' @param Dmatrix \code{matrix} A numeric distance matrix where columns represent data points.
#' @param match_num \code{numeric} The number of closest matches to return, based on Euclidean distance.
#'
#' @return A \code{data.frame} with two columns: \code{id}, representing the identifiers for each column in the matrix, and \code{diff}, which contains the calculated Euclidean distances, ordered from smallest to largest. The result is limited to the top \code{match_num} closest matches.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example usage
#'   Dmatrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
#'   match_num <- 2
#'   euclidean_n(Dmatrix, match_num)
#' }
euclidean_n <- function(Dmatrix,
                        match_num) {
  matching <<- Dmatrix %>%
    apply(2, norm, type = "2") %>%
    ## using Frobenius norm
    # apply(lb_sub, 2, norm, type = "f") %>%
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    arrange(diff) %>%
    slice(1:match_num)

  return(matching)
}


## 0.10 mahalanobis p ------------------
#' Mahalanobis Distance with P-value \eqn{\alpha} Criteria
#'
#' @description This function calculates the Mahalanobis distance for each column in a given distance matrix, filters the results based on a specified p-value threshold (\eqn{\alpha}), and returns a subset of distances that meet the criteria.
#'
#' @param Dmatrix \code{matrix} A numeric distance matrix where columns represent data points.
#' @param alpha \code{numeric} The p-value threshold (\eqn{\alpha}) used to filter the Mahalanobis distances. Only distances with p-values greater than or equal to \code{alpha} will be included in the result.
#'
#' @return A \code{data.frame} with two columns: \code{id}, representing the identifiers for each column in the matrix, and \code{diff}, which contains the calculated Mahalanobis distances, ordered by p-value. Only rows with p-values greater than or equal to \code{alpha} are returned.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example usage
#'   Dmatrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
#'   alpha <- 0.05
#'   mahalanobis_p(Dmatrix, alpha)
#' }
mahalanobis_p <- function(Dmatrix,
                          alpha) {
  def <- nrow(Dmatrix)
  df <- Dmatrix %>%
    ## Mahalanobis distance using the chisq pvalues
    as.matrix() %>%
    t()
  x <- sweep(df, 2L, 0)
  invcov <- MASS::ginv(cov(df))
  value <- rowSums(x %*% invcov * x)
  pvalue <- pchisq(value, df = def, lower.tail = FALSE)
  matching <<- data.frame(diff = value,
                          pvalue = as.numeric(pvalue)) %>%
    arrange(desc(pvalue)) %>%
    rownames_to_column("id") %>%
    as.data.frame() %>%
    dplyr::filter(pvalue >= alpha)

  # slice(1:match_num) %>%
  # inner_join(obs_data, by = "id")
  return(matching)
}

## 0.11 mahalanobis_n ---------------
#' Mahalanobis Distance with \eqn{\kappa} Criteria
#'
#' @description This function calculates the Mahalanobis distance for each column in a given distance matrix and returns the top \code{match_num} closest matches based on the smallest Mahalanobis distances.
#'
#' @param Dmatrix \code{matrix} A numeric distance matrix where columns represent data points.
#' @param match_num \code{numeric} The number of closest matches (\eqn{\kappa}) to return based on the Mahalanobis distance.
#'
#' @return A \code{data.frame} with two columns: \code{id}, representing the identifiers for each column in the matrix, and \code{diff}, which contains the calculated Mahalanobis distances, ordered from smallest to largest. The result is limited to the top \code{match_num} closest matches.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example usage
#'   Dmatrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
#'   match_num <- 2
#'   mahalanobis_n(Dmatrix, match_num)
#' }
mahalanobis_n <- function(Dmatrix,
                          match_num) {
  def <- nrow(Dmatrix)
  df <- Dmatrix %>%
    as.matrix() %>%
    t()
  x <- sweep(df, 2L, 0)
  invcov <- MASS::ginv(cov(df))
  value <- rowSums(x %*% invcov * x)

  matching <<- Dmatrix %>%
    t() %>%
    as.data.frame() %>%
    mutate(value = value) %>%
    arrange(value) %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    slice(1:match_num)

  return(matching)
}



