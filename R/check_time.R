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

  {rational + 1} %>%
  switch(floor(x), x) %>%
  structure(rational = rational)

}

# Pseudo-methods (avoiding base::NextMethod) ------------------------------

#' @export
check_time.integer <- function(x, ...) {
  check_time.numeric(x, FALSE)
}

#' @export
check_time.circular <- function( x, rational = attr(x, "rational"), ...) {
  check_time.numeric(x*60, rational)
}
