# Miscellaneous function -------------------------------------------------------

## this file contains some of the functions used in this project
## be careful use in other situations, not sure for correctness

## read_excel_allsheets --------------------------------------------------------
#' Title Read Multiple Sheets of Excel file
#'
#' @param filename the directory location of the excel file
#' @param tibble return the datasets in as.data.frame format
#'
#' @return a list of data.frames
#' @export
#'
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
#' Title Check whether the object is letters-only
#'
#' @param x the string element
#'
#' @return a Boolean result as TRUE or FALSE
#' @export
#'
letters_only <- function(x) {!grepl("[^A-Za-z]", x)}


## numbers_only ----------------------------------------------------------------
#' Title Check whether the object is number-only
#'
#' @param x the string element
#'
#' @return a Boolean result as TRUE or FALSE
#' @export
#'
numbers_only <- function(x) {!grepl("\\D", x)}


## package info ----------------------------------------------------------------
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\n Welcome to my package; this is a package
                        developed for Randy Jin's MS thesis")
}


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Randy",
    devtools.desc.author = "Randy Jin <xin.2.jin@cuanschutz.edu> [aut, cre]",
    devtools.desc.license = "What license is it under?",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}

## 0.1 norm L2 {{{--------------------------------------------------------------
#' Title L2 or other norms
#'
#' @param v any numeric vector
#' @return
norm2 <- function(v) {
  sqrt(sum(v^2))
}

## Add other type of norms???


## 0.2 not all na {{{-----------------------------------------------------------
#' Title Checking there is not all NA
#' @param x the target dataset
#' @return
not_all_na <- function(x) {
  any(!is.na(x))
}


## 0.3 not any na {{{-----------------------------------------------------------
#' Title Checking there is any not NA
#' @param x the target dataset
#' @return
not_any_na <- function(x) {
  all(!is.na(x))
}


## 0.4 not in {{{---------------------------------------------------------------
`%!in%` <- Negate(`%in%`)


# Distance ---------------------------------------------------------------------

## 0.5 euclidean_df ----------------------------------------------------------
#' Title: Calculate Euclidean distance for a matrix
#'
#' Calculates the Euclidean distance of a given data matrix,
#' at an arbitrary center.
#'
#' @param Dmatrix A numeric data matrix
#' @param center A numeric vector
#'
#' @return The Euclidean distance list order
#'         from the smallest to the largest difference
#' @export
#'
#' @examples
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
#' Title
#'
#' @param Dmatrix
#' @param center
#'
#' @return
#' @export
#'
#' @examples
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
#' Title Single time point matching with only matching time (replaced with the singletime_n)
#'
#' @param Dmatrix A distance matrix
#' @param match_time The matching anchor time as setup
#' @param center The original geographical center
#'
#' @return A distance list arranged from smallest to largest
#' @export
#'
#' @examples
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

## 0.7 singletime_n {{{---------------------------------------------------------
#' Title Single time point matching with both matching time and matching number
#'
#' @param Dmatrix
#' @param match_time
#' @param match_num
#'
#' @return
#' @export
#'
#' @examples
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



