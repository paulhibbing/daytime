range_fun <- function(x, limit, include_limit, include_fun, exclude_fun) {
  if (include_limit) include_fun(x, limit) else exclude_fun(x, limit)
}

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

range_examine <- function(
  x, lower = 0, upper = 1440,
  inc_lower = TRUE, inc_upper = FALSE,
  test_limits = TRUE
) {

  if (test_limits) order_limits(lower, upper)

  stats::na.omit(x) %>%
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

}

range_test <- function(
  x, lower, upper,
  inc_lower = TRUE, inc_upper = FALSE,
  rational_adjust = TRUE
) {

  order_limits(lower, upper)

  #* Set up the bounds and label

    if (rational_adjust) {

      stopifnot(!is.null(attr(x, "rational")))

      upper <- upper + attr(x, "rational")
      inc_upper <- !attr(x, "rational")

    }

    range_label <- paste0(
      switch(inc_lower+1, "(", "["), lower, ", ",
      upper, switch(inc_upper+1, ")", "]")
    )

  #* Run the test

    values_in_range <- range_examine(
      x, lower, upper, inc_lower, inc_upper, FALSE
    )

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
