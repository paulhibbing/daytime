# Package documentation ---------------------------------------------------

#' daytime: Operate on time variables for physical behavior research
#'
#' Crucial functions of the \code{daytime} package are to allow conversion of
#' timestamps to numeric time of day (e.g., \code{0} for midnight and
#' \code{1439} for 11:59) and subsequent calculation of circular mean and
#' standard deviation.
#'
#' @docType package
#' @name daytime
NULL


# Global Variables --------------------------------------------------------

if(getRversion() >= "2.15.1") utils::globalVariables(c("."))

# Imports -----------------------------------------------------------------

#' @importFrom magrittr %>% %T>% %<>%
NULL
