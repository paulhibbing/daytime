#' Retrieve daytime information from a timestamp
#'
#' @param x a timestamp that inherits from \code{character} or \code{POSIXt}
#' @param rational logical. Return partial minutes as a decimal addend (for
#'   numeric operations) or second value (for \code{strftime} operations)?
#' @param first_min value for the first minute of the day. Must be either
#'   \code{0} (default; minutes coded as 0-1439) or \code{1} (minutes coded as
#'   1-1440)
#' @param ... arguments passed to \code{as.POSIXct}
#'
#' @details If \code{x} is not already POSIX-formatted, coercion will be attempted.
#'   Arguments can be passed to \code{as.POSIXct} to assist with coercion,
#'   notably the \code{format} and \code{tz} arguments. Presumably the timezone
#'   should not matter, except perhaps during a daylight saving transition hour.
#'   But it is always safest to pass in POSIX-formatted data that you have
#'   vetted yourself.
#'
#'   These operations are essentially a thin wrapper around
#'   \code{\link{get_minute}}. The difference is intended use. \code{get_minute}
#'   is simply for retrieving the numeric values, while this function is for
#'   getting a class for which methods can be invoked (e.g. \code{mean}).
#'
#' @return an object with class \code{daytime}
#' @export
#'
#' @examples
#'
#' t1_char <- "2000-01-01 00:01:30"
#' t1 <- as.POSIXct(t1_char, "UTC")
#'
#' ## Methods exist to give comparable output from character and POSIX inputs
#' as.daytime(t1_char)
#' as.daytime(t1)
#'
#' ## Settings can be changed to yield a range of values
#' as.daytime(t1, TRUE)
#' as.daytime(t1, FALSE, 1)
#' as.daytime(t1, TRUE, 1)
#'
#' ## Beware of silent timezone changes. System will
#' ## assume your local timezone unless told otherwise!
#' as.daytime(
#'   strftime(t1, "%H:%M:%S", "EST"),
#'   format = "%H:%M:%S"
#' )
#'
as.daytime <- function(x, ...) {
  UseMethod("as.daytime", x)
}

#' @export
#' @rdname as.daytime
as.daytime.default <- function(x, ...) {
  stop(
    "No method exists to coerce object of class {",
    paste(class(x), collapse = ","), "} to daytime",
    call. = FALSE
  )
}

#' @export
#' @rdname as.daytime
as.daytime.POSIXt <- function(x, rational = FALSE, first_min = 0, ...) {
  get_minute(x, rational, first_min, ...) %>%
  structure(., class = append(class(.), "daytime", 0))
}

#' @export
#' @rdname as.daytime
as.daytime.character <- function(x, rational = FALSE, first_min = 0, ...) {
  as.daytime.POSIXt(x, rational, first_min, ...)
}
