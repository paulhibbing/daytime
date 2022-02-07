# Generic -----------------------------------------------------------------

#' Retrieve time of day in \code{\%H:\%M:\%S} or \code{\%H:\%M:00} format
#'
#' @param x a \code{daytime} or \code{POSIXt} object, or an object that can be
#'   cast to \code{daytime}
#' @param rational logical. Return partial minutes as a numeric \code{\%S}
#'   value? The default (\code{rational = FALSE}) will return \code{\%H:\%M:00}
#'   format.
#' @param ... arguments passed to \code{\link{as_daytime}}.
#'
#' @details For objects that do not inherit from \code{daytime} or
#'   \code{POSIXt}, the default method is a chain of casts, first to
#'   \code{daytime} and then to \code{character}.
#'
#' @return character value(s) in the desired format
#' @export
#'
#' @examples
#' x <- Sys.time()
#'
#' tod(x, FALSE)
#' tod(x, TRUE)
#'
#' tod(0, FALSE)
#' tod(720, FALSE)
#' tod(1439.999, TRUE)
tod <- function(x, ...) {
  UseMethod("tod", x)
}

# Methods -----------------------------------------------------------------

#' @export
#' @rdname tod
tod.default <- function(x, rational = attr(x, "rational"), ...) {

  check_rational(rational, x) %>%
  as_daytime(x, ., ...) %>%
  tod.daytime(.)

}

#' @export
#' @rdname tod
tod.daytime <- function(x, rational = attr(x, "rational"), ...) {

  check_time(x, rational) %>%
  {structure(as.POSIXct(
    as.numeric(.) * 60,
    lubridate::tz(Sys.Date()),
    origin = Sys.Date()
  ), rational = attr(., "rational"))} %>%
  strf_tod(.) %>%
  structure(x = attr(x, "x")) %>%
  attr_order(FALSE)

}

# Helper ------------------------------------------------------------------

strf_tod <- function(x, rational = attr(x, "rational")) {

  stopifnot(inherits(x, "POSIXt"))

  lubridate::tz(x) %>%
  strftime(x, switch(rational + 1, "%H:%M:00", "%H:%M:%S"), .) %>%
  structure(rational = rational)

}
