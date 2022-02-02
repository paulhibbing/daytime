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
#' y <- as_daytime(x)
#'
#' tod(x)
#' tod(y)
#' tod(x, rational = TRUE)
#'
#' tod(0)
#' tod(720)
#' tod(1440.9, 1, TRUE)
tod <- function(x, ...) {
  UseMethod("tod", x)
}

# Methods -----------------------------------------------------------------

#' @export
#' @rdname tod
tod.default <- function(x, rational = FALSE, ...) {

  as_daytime(x, rational, ...) %>%
  tod.daytime(.)

}

#' @export
#' @rdname tod
tod.daytime <- function(x, rational = attr(x, "rational"), ...) {

  {rational + 1} %>%
  switch(floor(x), x) %>%
  check_time(rational) %>%
  {as.POSIXct(
    . * 60,
    lubridate::tz(Sys.Date()),
    origin = Sys.Date()
  )} %>%
  tod.POSIXt(., attr(., "rational"))

}

#' @export
#' @rdname tod
tod.POSIXt <- function(x, rational = FALSE, ...) {

  lubridate::tz(x) %>%
  strftime(x, switch(rational + 1, "%H:%M:00", "%H:%M:%S"), .) %>%
  structure(., x = x, rational = rational)

}
