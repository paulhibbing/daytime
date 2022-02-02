# Methods -----------------------------------------------------------------

#' Methods for class \code{daytime}
#'
#' @param x a daytime object
#' @param ... arguments passed to methods (currently unused)
#'
#' @export
#' @examples
#'
#' Time <- as_daytime(Sys.time()+rnorm(100, 2*1440, 2*60), TRUE)
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
      tod(mean, attr(mean, "rational"), attr(mean, "first_min")),
      " \u00B1 ", format(sd, ...)
    )
  }) %>%
  {if (give_df) . else .$sum_string}

}
