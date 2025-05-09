% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00_utils.R
\name{.onAttach}
\alias{.onAttach}
\title{Check if a String Contains Only Letters}
\usage{
.onAttach(libname, pkgname)
}
\arguments{
\item{x}{\code{character} The input string to be checked.}
}
\value{
\code{logical} \code{TRUE} if the string contains only letters, otherwise \code{FALSE}.

\code{logical} \code{TRUE} if the string contains only numeric characters, otherwise \code{FALSE}.
}
\description{
This function checks whether the input string contains only alphabetic characters (A-Z, a-z), returning \code{TRUE} if the string contains only letters and \code{FALSE} otherwise.

This function checks whether the input string contains only numeric characters (0-9), returning \code{TRUE} if the string contains only numbers and \code{FALSE} otherwise.
}
\examples{
\dontrun{
  # Check if a string contains only letters
  letters_only("HelloWorld")  # TRUE
  letters_only("Hello123")    # FALSE
}
Check if a String Contains Only Numbers

\dontrun{
  # Check if a string contains only numbers
  numbers_only("123456")   # TRUE
  numbers_only("123abc")   # FALSE
}
}
