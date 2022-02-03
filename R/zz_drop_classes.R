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

drop_circular <- function(x, warn_circular = TRUE, collapse = FALSE) {

  if (!inherits(x, "circular")) {
    x
  } else {

    x %T>%
    {if (warn_circular) warning(
      "Removing class `circular`", call. = FALSE
    )} %>%
    structure(
      circularp = NULL,
      class = setdiff(class(.), "circular")
    ) %>%
    circ_to_char(collapse)

  }

}

circ_to_char <- function(x, collapse = FALSE) {

  if (!collapse) return(x)

  format(x, digits = 1, nsmall = 1) %>%
  paste(collapse = ", ") %>%
  paste0("Circular: ", .)

}
