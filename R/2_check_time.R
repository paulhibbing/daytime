# Generic -----------------------------------------------------------------

check_time <- function(x, ...) {
  UseMethod("check_time", x)
}

# Methods -----------------------------------------------------------------

#' @export
check_time.POSIXt <- function(x, ...) {
  x
}

#' @export
check_time.character <- function(x, ...) {
  as.POSIXct(x, ...)
}

#' @export
check_time.numeric <- function(
  x, first_min = attr(x, "first_min"),
  rational = attr(x, "first_min"), ...
) {

  rational %<>% check_rational(x)

  if (
    isTRUE(is.null(first_min)) |
    isTRUE(!first_min %in% 0:1)
  ) stop(
    "A value of 0 or 1 must be provided for the first_min argument",
    call. = FALSE
  )

  if (!range_test(x, first_min)) stop(
    "Found time values outside the expected range of [",
    first_min, ",", first_min+1440, ")\nPlease recalculate",
    " (perhaps using `x%%1440`, with caution) and try again",
    call. = FALSE
  )

  structure(x, first_min = first_min, rational = rational)

}

# Pseudo-methods (avoiding base::NextMethod) ------------------------------

#' @export
check_time.integer <- function(
  x, first_min = attr(x, "first_min"), ...
) {
  check_time.numeric(x, first_min, FALSE)
}

#' @export
check_time.circular <- function(
  x, first_min = attr(x, "first_min"),
  rational = attr(x, "rational"),  ...
) {

  rational %<>% check_rational(x)

  {rational + 1} %>%
  switch(floor(x*60), x*60) %>%
  check_time.numeric(first_min, rational)

}
