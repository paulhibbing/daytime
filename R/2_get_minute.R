# Dispatch-alike function  ------------------------------------------------

#' Convert timestamp to minute of day
#'
#' This is a generic-like function that will extract the numeric minute of day
#' if \code{x} inherits from \code{character} or \code{POSIXt}, or will
#' perform the reverse operation (returning a string with \code{\%H:\%M:\%S}
#' format) if \code{x} inherits from \code{integer} or \code{numeric}.
#'
#' @inheritParams as.daytime
#'
#' @note The value of \code{first_min} is stored as an attribute of the output
#'   when \code{x} inherits from \code{character} or \code{POSIXt}. This value
#'   is referenced when attempting re-conversion, and a warning will be issued
#'   if there is a conflict between the attribute and what is passed by the
#'   user. (See the commented example below.)
#'
#' @export
#'
#' @examples
#'
#' ## Extracting the numeric minute
#' Time <- Sys.time()
#' Time
#' get_minute(Time)
#' get_minute(Time, TRUE)
#' get_minute(Time, TRUE, 1)
#'
#' ## Reversing the operation
#' daytime <- get_minute(Time, TRUE)
#' get_minute(daytime)
#' get_minute(daytime, TRUE)
#' # get_minute(daytime, TRUE, 1)
get_minute <- function(x, rational = FALSE, first_min = 0, ...) {

  if (inherits(x, c("integer", "numeric"))) {

    get_minute_num(x, rational, first_min)

  } else if (inherits(x, c("character", "POSIXt"))) {

    get_minute_ts(x, rational, first_min, ...)

  } else {

    stop(
      "Don\'t know how to perform the `get_minute` operation on",
      " an object of class {", paste(class(x), collapse = ","), "}"
    )

  }

}

# Method-alike functions --------------------------------------------------

get_minute_ts <- function(x, rational = FALSE, first_min = 0, ...) {

  check_time_ts(x, first_min, ...) %>%
  {. - lubridate::floor_date(., "days")} %>%
  as.numeric("mins") %>%
  {. - ((. - floor(.)) * !rational)} %>%
  {. + first_min} %>%
  structure(timestamp = x, rational = rational, first_min = first_min)

}

get_minute_num <- function(x, rational = FALSE, first_min = 0, ...) {

  {rational + 1} %>%
  switch(floor(x), x) %>%
  check_time_num(first_min) %>%
  {as.POSIXct(
    (.$timestamp - .$first_min) * 60,
    lubridate::tz(Sys.Date()),
    origin = Sys.Date()
  )} %>%
  strftime(., "%H:%M:%S", lubridate::tz(.)) %>%
  structure(timestamp = x, rational = rational, first_min = first_min)

}

# Internal helpers --------------------------------------------------------

check_time_ts <- function (x, first_min, ...) {

  stopifnot(
    inherits(x, c("character", "POSIXt")),
    first_min %in% 0:1
  )

  if (is.character(x)) {
    as.POSIXct(x, ...)
  } else {
    x
  }

}

range_test <- function(x, first_min) {

  x %>%
  cut(
    c(-Inf, first_min, first_min + 1440, Inf),
    c("out_of_bounds", "in_bounds", "out_of_bounds"),
    right = FALSE
  ) %>%
  {. %in% c("in_bounds", NA)} %>%
  all(.)

}

check_time_num <- function(x, first_min) {

  stopifnot(
    inherits(x, c("integer", "numeric")),
    all(stats::na.omit(x) >= 0),
    first_min %in% 0:1
  )

  if (!range_test(x, first_min)) {
    stop(
      "Found time values outside the expected range of [",
      first_min, ",", first_min+1440, ")\nPlease recalculate",
      " (perhaps using `x%%1440`, with caution) and try again"
    )
  }

  if (
    !is.null(attr(x, "first_min")) &
    isTRUE(attr(x, "first_min") != first_min)
  ) {

    warning(
      "Overriding `first_min` with the attribute value in `x`",
      call. = FALSE
    )

    first_min <- attr(x, "first_min")

  }

  list(timestamp = x, first_min = first_min)

}
