#' Methods for class \code{daytime}
#'
#' @param x a daytime object
#' @param ... arguments passed to methods (currently unused)
#'
#' @export
#' @examples
#'
#' Time <- as_daytime(
#'   Sys.time()+rnorm(100, 2*1440, 12*60), TRUE
#' )
#'
#' ## Wrap in `as.numeric` for better printing
#' as.numeric(mean(Time))
#' as.numeric(sd(Time))
#'
#' ## Compare
#' mean(as.numeric(Time))
#' sd(as.numeric(Time))
#' PAutilities::mean_sd(Time)
#'
#' @name daytime_methods
mean.daytime <- function(x, ...) {

  as_circular(x) %>%
  attr_apply(
    mean, lower = -24, upper = 24,
    inc_lower = FALSE, inc_upper = FALSE,
    rational_adjust = FALSE
  ) %>%
  drop_circular(FALSE) %>%
  structure(
    .,
    x = drop_circular(attr(., "x"), FALSE, TRUE)
  ) %>%
  hr_to_min(.)

}

#' @rdname daytime_methods
#' @export sd.daytime
#' @export
sd.daytime <- function(x, ...) {

  as_circular(x) %>%
  attr_apply(
    sd, lower = -2*pi, upper = 2*pi,
    inc_lower = FALSE, inc_upper = FALSE,
    rational_adjust = FALSE
  ) %>% ## gives radians (?)
  {. * (12/pi)} %>% ## convert to hrs
  drop_circular(FALSE) %>%
  structure(
    .,
    x = drop_circular(attr(., "x"), FALSE, TRUE)
  ) %>%
  hr_to_min(.)

}

#' @export
mean_sd.daytime <- function(
  x = NULL, MoreArgs = NULL, give_df = TRUE,
  units = c("min", "hr"), ..., mean_x = NULL, sd_x = NULL
) {

  units <- match.arg(units)

  data.frame(mean = mean(x), sd = sd(x)) %>%
  within({
    sd = switch(units, "min" = sd, "hr" = sd / 60, NULL)
    sum_string = paste0(
      tod(mean, attr(mean, "rational")),
      " \u00B1 ", format(sd, ...)
    )
  }) %>%
  {if (give_df) . else .$sum_string}

}

#' Test if an object belongs to \code{daytime} class
#'
#' @param x object to test
#' @param ... arguments passed to \code{\link{range_test}}
#'
#' @export
#'
#' @examples
#' x <- as_daytime(Sys.time(), FALSE)
#' is.daytime(x)
is.daytime <- function(x, ...) {

  all(
    inherits(x, "daytime", TRUE) == 1,
    !is.null(attr(x, "x")),
    isTRUE(is.logical(attr(x, "rational"))),
    range_test(x, ...)
  )

}

print.daytime <- function(x, ...) {

  if (circular::is.circular(attr(x, "x"))) {
    attr(x, "x") %<>% drop_circular(FALSE, TRUE)
  }

  if (circular::is.circular(x)) {
    print(x, info = FALSE)
  } else {
    print(x, ...)
  }

}

#' Plot a \code{daytime} object
#'
#' @param x a \code{daytime} object
#' @param ... arguments passed to \code{\link{as_circular}}
#'
#' @details \code{x} is first cast as \code{circular}, then forwarded to the
#'   plot method for \code{circular} objects
#'
#' plot(as_daytime(
#'   seq(
#'     as.POSIXct(Sys.Date()),
#'     as.POSIXct(Sys.Date()+1),
#'     "3 hour"
#'    )
#' ))
#'
#' @export
plot.daytime <- function(x, ...) {
  as_circular(x, ...) %>%
  plot(.)
}
