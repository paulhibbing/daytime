# as_daytime --------------------------------------------------------------

  check_rational <- function(rational, x) {

    if (!is.null(attr(x, "rational"))) {

      stopifnot(
        is.logical(rational),
        is.logical(attr(x, "rational"))
      )

      if (rational != attr(x, "rational")) warning(
        "Conflict detected between attr(x, \"rational\") {",
        attr(x, "rational"), "} and user input {rational = ", rational,
        "}.\nUser input will be ignored.", call. = FALSE
      )

      rational <- attr(x, "rational")

    } else if (is.null(rational)) {
      rational <- !isTRUE(all.equal(x, as.integer(x)))
      warning("Setting rational to ", rational, call. = FALSE)
    } else {
      stopifnot(isTRUE(is.logical(rational)))
    }

    rational

  }

  structure_daytime <- function(
    new_x, x, first_min = attr(new_x, "first_min"),
    rational = attr(new_x, "rational")
  ) {

    class(new_x) %>%
    append("daytime", 0) %>%
    unique(.) %>%
    structure(
      new_x, x = drop_daytime(x), class = .,
      first_min = if (is.null(first_min)) {
        warning("Setting `first_min` to 0", call. = FALSE)
        0
      } else {
        first_min
      },
      rational = if (is.null(rational)) {
        warning("Setting `rational` to FALSE", call. = FALSE)
        FALSE
      } else {
        rational
      }
    ) %>%
    attr_order(.)

  }

  drop_daytime <- function(x) {

    if (!inherits(x, "daytime")) {
      x
    } else {
      x %<>% structure(
        x = NULL, first_min = NULL, rational = NULL,
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
      c("x", "first_min", "rational", "class") %>%
      {c(setdiff(names(a), .), .)} %>%
      a[.]
    x
  }

# methods.daytime ---------------------------------------------------------

  attr_apply <- function(x, f) {
    f(x) %>%
    structure(
      .,
      rational = attr(x, "rational"),
      first_min = attr(x, "first_min"),
      class = unique(c(
        "daytime",
        setdiff(class(.), "circular")
      ))
    )
  }

  hr_to_min <- function(hr) {
    {hr * 60} %>%
    {. + 1440} %>%
    {. %% 1440}
  }

# Misc --------------------------------------------------------------------

  is.daytime <- function(x, ...) {
    all(
      inherits(x, "daytime", TRUE) == 1,
      !is.null(attr(x, "x")),
      isTRUE(is.logical(attr(x, "rational"))),
      isTRUE(attr(x, "first_min") %in% 0:1)
    )
  }

  range_test <- function(
    x, first_min, lower = first_min, upper = first_min + 1440
  ) {

    if (!missing(first_min)) stopifnot(
      first_min %in% 0:1
    )

    stats::na.omit(x) %>%
    {. >= lower & . < upper} %>%
    all(.)

  }
