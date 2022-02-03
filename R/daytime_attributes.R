structure_daytime <- function(
  new_x, x,  rational = attr(new_x, "rational"),
  ...
) {

  class(new_x) %>%
  append("daytime", 0) %>%
  unique(.) %>%
  structure(
    new_x, x = drop_daytime(x), class = .,
    rational = if (is.null(rational)) {
      warning("Setting `rational` to FALSE", call. = FALSE)
      FALSE
    } else {
      rational
    }
  ) %>%
  attr_order(...) ## Includes a check for is.daytime

}

attr_order <- function(x, ...) {

  stopifnot(is.daytime(x, ...))

  a <-
    attributes(x) %>%
    .[!duplicated(.)]

  attributes(x) <-
    c("x", "rational", "class") %>%
    {c(setdiff(names(a), .), .)} %>%
    a[.]

  x

}
