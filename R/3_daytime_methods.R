# Methods -----------------------------------------------------------------

#' Methods for class \code{daytime}
#'
#' @param x a daytime object
#' @param ... arguments passed to methods (currently unused)
#'
#' @export
#' @examples
#'
#' Time <- as.daytime(Sys.time()+rnorm(100, 2*1440, 2*60), TRUE)
#'
#' ## Wrap in `as.numeric` for better printing
#' as.numeric(mean(Time))
#' as.numeric(sd(Time))
#'
#' ## Compare
#' mean(as.numeric(Time))
#' sd(as.numeric(Time))
#'
#' @name daytime_methods
mean.daytime <- function(x, ...) {

  as_circular(x) %>%
  attr_apply(mean) %>%
  hr_to_min(.)

}

#' @rdname daytime_methods
#' @export sd.daytime
#' @export
sd.daytime <- function(x, ...) {

  as_circular(x) %>%
  attr_apply(sd) %>% ## gives radians (?)
  {. * (12/pi)} %>% ## convert to hrs
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
      get_minute(mean, attr(mean, "rational"), attr(mean, "first_min")),
      " \u00B1 ", format(sd, ...)
    )
  }) %>%
  {if (give_df) . else .$sum_string}

}

# Helpers (units/attributes) ----------------------------------------------

hr_to_min <- function(hr) {
  {hr * 60} %>%
  {. + 1440} %>%
  {. %% 1440}
}

attr_apply <- function(x, f) {
  f(x) %>%
  structure(
    .,
    rational = attr(x, "rational"),
    first_min = attr(x, "first_min"),
    class = unique(c("daytime", class(.)))
  )
}

# Helpers (coercion) ------------------------------------------------------

as_circular <- function(x, ...) {
  UseMethod("as_circular", x)
}

#' @export
as_circular.default <- function(x, ...) {
  stop(
    "No method exists to coerce object of class {",
    paste(class(x), collapse = ","), "} to circular",
    call. = FALSE
  )
}

#' @export
as_circular.daytime <- function(x, ...) {

  if (inherits(x, "circular")) {
    return(structure(
      x, class = unique(c("circular", class(x)))
    ))
  }

  if (attr(x, "first_min") == 0) {
    x %<>% {. + 1}
  }

  out_of_range <- !data.table::inrange(x, 1, 1440)
  if (any(out_of_range)) {
    stopifnot(attr(x, "rational"))
    warning(
      "Detected ", sum(out_of_range), " element(s) of `x` that",
      " fall outside the expected range of [1,1440].\nThey will be",
      " rounded into that range. Avoid this warning by setting",
      " `rational=FALSE`\nin the original call to `as.daytime`",
      call. = FALSE
    )

  }

  pmax(x, 1) %>%
  pmin(1440) %>%
  {circular::circular(
    ./60, units = "hours", template = "clock24"
  )} %>%
  structure(
    timestamp = attr(x, "timestamp"),
    rational = attr(x, "rational"),
    first_min = attr(x, "first_min")
  )

}
