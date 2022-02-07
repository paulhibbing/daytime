structure_daytime <- function(
  new_x, x,  rational = attr(new_x, "rational"),
  ...
) {

  class(new_x) %>%
  append("daytime", 0) %>%
  unique(.) %>%
  structure(
    new_x, x = drop_daytime(x), class = .,
    rational = check_rational(rational, new_x)
  ) %>%
  attr_order(TRUE, ...)

}

attr_order <- function(x, test_daytime, ...) {

  stopifnot(is.logical(test_daytime), !is.na(test_daytime))

  if (test_daytime) stopifnot(is.daytime(x, ...))

  a <-
    attributes(x) %>%
    .[!duplicated(.)]

  attributes(x) <-
    c("x", "rational", "class") %>%
    {c(setdiff(names(a), .), .)} %>%
    a[.]

  x

}
