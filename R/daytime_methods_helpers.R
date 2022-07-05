# Setting up S3 for `sd` --------------------------------------------------

  #' @rdname daytime_methods
  #' @param x input data
  #' @param ... arguments passed to methods
  #' @export
  sd <- function(x, ...) UseMethod("sd")

  #' @export
  sd.default <- function(x, na.rm = FALSE, ...) stats::sd(x, na.rm)


# Sub-methods for sd.daytime ----------------------------------------------

  msd <- function(x, units) {

    mean(x) %>%
    {abs(x - .)} %>%
    as.numeric(.) %>%
    pmin(., 1440 - .) %>%
    structure(., rational = attr(x, "rational")) %>%
    attr_apply(mean) %>%
    sd_units(units, "min")

  }

  srl <- function(x, units) {

    as_circular(x) %>%
    circular::rho.circular(.) %>%
    {1 - .} %>%
    {. * 12} %>%
    sd_units(units, "hr")

  }


# Some general-purpose helpers --------------------------------------------

  sd_units <- function(s, units, current) {
    switch(
      units,
      "min" = if (current == "min") s else s * 60,
      "hr"  = if (current == "hr")  s else s / 60,
      stop("Invalid units selected")
    )
  }

  attr_apply <- function(x, f, ...) {
    f(x) %>%
    structure_daytime(., x, attr(x, "rational"), ...)
  }

  hr_to_min <- function(hr) {
    {hr * 60} %>%
    {. + 1440} %>%
    {. %% 1440}
  }
