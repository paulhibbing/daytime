structure_daytime <- function(
  new_x, x,  rational = attr(new_x, "rational")
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
  attr_order(.) %T>%
  {stopifnot(is.daytime(.))}

}

drop_daytime <- function(x) {

  if (!inherits(x, "daytime")) {

    x

  } else {

    x %<>% structure(
      x = NULL, rational = NULL,
      class = setdiff(class(x), "daytime")
    )

    attributes(x) %<>% {c(
      .[setdiff(names(.), "class")],
      class = list(.$class)
    )}

    x

  }

}

attr_order <- function(x) {

  stopifnot(is.daytime(x))

  a <- attributes(x)

  attributes(x) <-
    c("x", "rational", "class") %>%
    {c(setdiff(names(a), .), .)} %>%
    a[.]

  x

}
