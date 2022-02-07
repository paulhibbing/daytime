#' @rdname range_test
range_fun <- function(x, limit, include_limit, include_fun, exclude_fun) {
  if (include_limit) include_fun(x, limit) else exclude_fun(x, limit)
}

#' @rdname range_test
order_limits <- function(lower, upper) {

  if (lower > upper) {

    warning(
      "lower is greater than upper; they will be swapped",
      call. = FALSE
    )

    assign("lower", upper, parent.frame())
    assign("upper", lower, parent.frame())

  }

  invisible()

}

#' @rdname range_test
range_examine <- function(
  x, lower = 0, upper = 1440,
  inc_lower = TRUE, inc_upper = FALSE,
  test_limits = TRUE
) {

  if (test_limits) order_limits(lower, upper)

  x %<>% stats::na.exclude(.)

  valid_tests <-
    drop_daytime(x) %>%
    {
      range_fun(
        ., lower, inc_lower,
        base::`>=`, base::`>`
      ) &
      range_fun(
        ., upper, inc_upper,
        base::`<=`, base::`<`
      )
    }

  if (!is.null(attr(x, "na.action"))) {
    indices <- rep(NA, length(x) + length(attr(x, "na.action")))
    indices[-attr(x, "na.action")] <- seq(valid_tests)
    valid_tests %<>% .[indices]
  }

  valid_tests

}

#' Test object ranges against expectations
#'
#' @param x object to test
#' @param lower lower limit of expectation
#' @param upper upper limit of expectation
#' @param inc_lower logical. Include \code{lower} in the range (\code{[}) or
#'   not (\code{(})?
#' @param inc_upper logical. Include \code{upper} in the range (\code{]}) or not
#'   (\code{)})?
#' @param rational_adjust logical. Adjust the range based on a \code{rational}
#'   attribute in \code{x}?
#' @param limit placeholder for \code{lower} or \code{upper}
#' @param include_fun function for comparing \code{x} against \code{limit} when
#'   the limit is included in the range
#' @param exclude_fun function for comparing \code{x} against \code{limit} when
#'   the limit is excluded from the range
#' @param test_limits logical. run \code{order_limits} to check that
#'   \code{lower} is less than \code{upper}
#'
#' @keywords internal
range_test <- function(
  x, lower = 0, upper = 1439,
  inc_lower = TRUE, inc_upper = FALSE,
  rational_adjust = TRUE
) {

  order_limits(lower, upper)

  #* Set up the bounds and label

    if (rational_adjust) {

      if (is.null(attr(x, "rational"))) {
        warning("Skipping rational_adjust (no `rational` attribute)")
      } else {
        upper <- upper + attr(x, "rational")
        inc_upper <- !attr(x, "rational")
      }

    }

    range_label <- paste0(
      switch(inc_lower+1, "(", "["), lower, ", ",
      upper, switch(inc_upper+1, ")", "]")
    )

  #* Run the test

    values_in_range <-
      stats::na.omit(x) %>%
      range_examine(lower, upper, inc_lower, inc_upper, FALSE)

    if (any(!values_in_range)) {

      stop(
        "Detected ", sum(!values_in_range), " element(s) of `x` that",
        " fall outside the expected range of ", range_label,  ".",
        call. = FALSE
      )

    }

  #* Return (There is no FALSE -- just failure)

    TRUE

}
