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
check_time.numeric <- function(x, rational = attr(x, "rational"), ...) {

  rational %<>% check_rational(x)

  if (!range_test(x)) stop(
    "Found time values outside the expected range of [0, 1439)",
    "\nPlease recalculate (perhaps using `x%%1440`, with caution)",
    " and try again", call. = FALSE
  )

  structure(x, rational = rational)

}

# Pseudo-methods (avoiding base::NextMethod) ------------------------------

#' @export
check_time.integer <- function(x, ...) {
  check_time.numeric(x, FALSE)
}

#' @export
check_time.circular <- function( x, rational = attr(x, "rational"), ...) {

  rational %<>% check_rational(x)

  {rational + 1} %>%
  switch(floor(x*60), x*60) %>%
  check_time.numeric(rational)

}
