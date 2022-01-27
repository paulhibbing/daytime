#' Retrieve daytime information from a timestamp
#'
#' @param x a timestamp that inherits from \code{character} or \code{POSIXt}
#' @param rational logical. Return partial minutes as a decimal addend?
#' @param first_min value for the first minute of the day. Must be either
#'   \code{0} (default; minutes coded as 0-1439) or \code{1} (minutes coded as
#'   1-1440)
#' @param ... arguments passed to \code{as.POSIXct}
#'
#' @note If \code{x} is not already POSIX-formatted, coercion will be attempted.
#'   Arguments can be passed to \code{as.POSIXct} to assist with coercion,
#'   notably the \code{format} and \code{tz} arguments. Presumably the timezone
#'   should not matter, except perhaps during a daylight saving transition hour.
#'   But it is always safest to pass in POSIX-formatted data that you have
#'   vetted yourself.
#'
#' @return an object with class \code{daytime}
#' @export
#'
#' @examples
#'
#' Time <- "2000-01-01 00:01:30"
#' t1 <- as.POSIXct(Time, "UTC")
#' t2 <- as.POSIXct(Time, "EST")
#' as.daytime(Time)
#' as.daytime(t1, TRUE)
#' as.daytime(t2, TRUE, 1)
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
    "No method to coerce object of class {",
    paste(class(x), sep = ","), " to daytime",
    call. = FALSE
  )
}

#' @export
#' @rdname as.daytime
as.daytime.POSIXt <- function(x, rational = FALSE, first_min = 0, ...) {
  get_minute(x, rational, first_min, ...) %>%
  structure(timestamp = x)
}

#' @export
#' @rdname as.daytime
as.daytime.character <- function(x, rational = FALSE, first_min = 0, ...) {
  as.daytime.POSIXt(x, rational, first_min, ...)
}
