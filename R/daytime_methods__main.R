#' Methods for class \code{daytime}
#'
#' @param x a daytime object
#' @param ... arguments passed to methods (currently unused)
#'
#' @export
#' @examples
#'
#' Time <- as_daytime(
#'   Sys.time()+rnorm(100, 12*60*60, 12*60*60), TRUE
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
sd.daytime <- function(x, units = c("min", "hr"), ...) {

  units <- match.arg(units)

  mean(x) %>%
  {abs(x - .)} %>%
  as.numeric(.) %>%
  pmin(., 1440 - .) %>%
  structure(., rational = attr(x, "rational")) %>%
  attr_apply(mean) %>%
  {switch(units, "min" = ., "hr" = . / 60, NULL)}

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

#' @export
c.daytime <- function(...) {

  obs <-
    list(...) %T>%
    {stopifnot(all(sapply(., is.daytime)))}

  rational <-
    sapply(obs, attr, "rational") %>%
    unique(.)

  if (length(rational) != 1) {
    warning("Setting rational to TRUE", call. = FALSE)
    rational <- TRUE
  }

  x_classes <-
    lapply(obs, attr, "x") %>%
    lapply(class) %>%
    lapply(gsub, pattern = "^integer$", replacement = "numeric") %>%
    unique(.)

  if (length(x_classes) != 1) {
    warning(
      "Converting all `x` attributes to character for concatenation",
      call. = FALSE
    )
    x <-
      lapply(obs, attr, "x") %>%
      lapply(as.character) %>%
      sapply(paste, collapse = ", ")
  } else {
    x <-
      lapply(obs, attr, "x") %>%
      do.call(c, .)
  }

  unlist(obs) %>%
  structure_daytime(x, rational)

}

#' @export
print.daytime <- function(x, ...) {

  if (circular::is.circular(attr(x, "x"))) {
    attr(x, "x") %<>% drop_circular(FALSE, TRUE)
  }

  NextMethod()

}
