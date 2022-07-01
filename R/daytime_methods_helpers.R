#' @rdname daytime_methods
#' @param x input data
#' @param ... arguments passed to methods
#' @export
sd <- function(x, ...) UseMethod("sd")

#' @export
sd.default <- function(x, na.rm = FALSE, ...) stats::sd(x, na.rm)

attr_apply <- function(x, f, ...) {
  f(x) %>%
  structure_daytime(., x, attr(x, "rational"), ...)
}

hr_to_min <- function(hr) {
  {hr * 60} %>%
  {. + 1440} %>%
  {. %% 1440}
}
