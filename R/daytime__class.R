# Generic -----------------------------------------------------------------

#' Retrieve minute of day
#'
#' @param x value from which to determine the minute of day value; typically
#'   inherits from \code{character} or \code{POSIXt}, but methods are available
#'   for \code{circular} and \code{numeric} as well
#' @param rational logical. If \code{FALSE}, values are rounded down to the
#'   nearest whole minute; if \code{TRUE}, real-numbered values are returned
#'   with the decimal portions reflecting seconds
#' @param warn_circular logical. Issue a warning if class \code{circular} is
#'   being removed?
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
#' t1_char <- "00:01:30"
#' t1 <- as.POSIXct(t1_char, "UTC", format = "%H:%M:%S")
#'
#' ## Methods exist to give comparable output from character and POSIX inputs
#' as_daytime(t1_char, format = "%H:%M:%S")
#' as_daytime(t1)
#'
#' as_daytime(t1, TRUE)
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
as_daytime.POSIXt <- function(x, rational = FALSE, ...) {
  check_time(x, ...) %>%
  {. - lubridate::floor_date(., "days")} %>%
  as.numeric("mins") %>%
  {. - ((. - floor(.)) * !rational)} %>%
  structure_daytime(., x, rational)
}


#' @export
#' @rdname as_daytime
as_daytime.numeric <- function(x, rational = attr(x, "rational"), ...) {
  check_time(x, rational) %>%
  structure_daytime(., x)
}

#' @export
#' @rdname as_daytime
as_daytime.circular <- function(
  x, rational = attr(x, "rational"), warn_circular = TRUE, ...
) {

  if (attr(x, "circularp")$units != "hours") stop(
    "Expecting circular object with units==\"hours\""
  )

  if (any(!range_examine(x, 0, 24))) stop(
    "Expecting circular object with all values",
    " in the interval [0, 24)"
  )

  check_time(x, rational) %>%
  drop_circular(warn_circular) %>%
  structure_daytime(., x)

}

# Pseudo-methods (avoiding base::NextMethod) ------------------------------

#' @export
#' @rdname as_daytime
as_daytime.character <- function(x, rational = FALSE, ...) {
  as_daytime.POSIXt(x, rational, ...)
}

#' @export
#' @rdname as_daytime
as_daytime.integer <- function(x, ...) {
  as_daytime.numeric(x, FALSE)
}
