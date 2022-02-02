# Generic -----------------------------------------------------------------

#' Retrieve minute of day
#'
#' @param x value from which to determine the minute of day value; typically
#'   inherits from \code{character} or \code{POSIXt}, but methods are available
#'   for \code{circular} and \code{numeric} as well
#' @param first_min value for the first minute of the day. Must be either
#'   \code{0} (minutes coded as 0-1439) or \code{1} (minutes coded as 1-1440)
#' @param rational logical. If \code{FALSE}, values are rounded down to the
#'   nearest whole minute; if \code{TRUE}, real-numbered values are returned
#'   with the decimal portions reflecting seconds
#' @param ... arguments passed to \code{as.POSIXct}. See details.
#'
#' @details If \code{x} inherits from \code{character}, casting to \code{POSIXt}
#'   will be attempted. Arguments can be passed to \code{as.POSIXct} to assist
#'   with this, notably the \code{format} and \code{tz} arguments. Presumably
#'   the timezone should not matter, except perhaps during a daylight saving
#'   transition hour. But it is always safest to pass in POSIX-formatted data
#'   that you have vetted yourself.
#'
#' @return a \code{daytime} object
#' @export
#'
#' @examples
#'
#' t1_char <- "2000-01-01 00:01:30"
#' t1 <- as.POSIXct(t1_char, "UTC")
#'
#' ## Methods exist to give comparable output from character and POSIX inputs
#' as_daytime(t1_char)
#' as_daytime(t1)
#'
#' ## Settings can be changed to yield a range of values
#' as_daytime(t1, 0, TRUE)
#' as_daytime(t1, 1, FALSE)
#' as_daytime(t1, 1, TRUE)
#'
#' ## Beware of silent timezone changes. System will
#' ## assume your local timezone unless told otherwise!
#' as_daytime(
#'   strftime(t1, "%H:%M:%S", "EST"),
#'   format = "%H:%M:%S"
#' )
#'
as_daytime <- function(x, ...) {
  UseMethod("as_daytime", x)
}

# Methods -----------------------------------------------------------------

#' @export
#' @rdname as_daytime
as_daytime.POSIXt <- function(x, first_min = 0, rational = FALSE, ...) {
  check_time(x, ...) %>%
  {. - lubridate::floor_date(., "days")} %>%
  as.numeric("mins") %>%
  {. - ((. - floor(.)) * !rational)} %>%
  {. + first_min} %>%
  structure_daytime(., x, first_min, rational)
}


#' @export
#' @rdname as_daytime
as_daytime.numeric <- function(
  x, first_min = attr(x, "first_min"),
  rational = attr(x, "rational"), ...
) {
  check_time(x, first_min, rational) %>%
  structure_daytime(., x)
}

#' @export
#' @rdname as_daytime
as_daytime.circular <- function(
  x, first_min = attr(x, "first_min"),
  rational = attr(x, "rational"), ...
) {

  if (attr(x, "circularp")$units != "hours") stop(
    "Expecting circular object with units==\"hours\""
  )

  if (!range_test(x, lower = 0/60, upper = 1441/60)) stop(
    "Expecting circular object with all values",
    " in the interval [0/60, 1441/60)"
  )

  warning(
    "Removing class `circular` for conversion to `daytime`",
    call. = FALSE
  )

  check_time(x, first_min, rational) %>%
  structure(
    circularp = NULL,
    class = setdiff(class(.), "circular")
  ) %>%
  structure_daytime(., x)

}

# Pseudo-methods (avoiding base::NextMethod) ------------------------------

#' @export
#' @rdname as_daytime
as_daytime.character <- function(x, first_min = 0, rational = FALSE, ...) {
  as_daytime.POSIXt(x, first_min, rational, ...)
}

#' @export
#' @rdname as_daytime
as_daytime.integer <- function(x, first_min = attr(x, "first_min"), ...) {
  as_daytime.numeric(x, first_min, FALSE)
}
