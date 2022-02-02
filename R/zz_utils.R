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

# methods.daytime ---------------------------------------------------------

  attr_apply <- function(x, f) {
    f(x) %>%
    structure(
      .,
      rational = attr(x, "rational"),
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

    #* Test elements

      ok <- all(
        inherits(x, "daytime", TRUE) == 1,
        !is.null(attr(x, "x")),
        isTRUE(is.logical(attr(x, "rational")))
      )

    #* Test range

      upper <- 1439 + attr(x, "rational")
      inc_upper <- !attr(x, "rational")

      values_in_range <- range_test(x, 0, upper, TRUE, inc_upper)

      range_label <- paste0(
        "[0, ", upper, switch(inc_upper+1, ")", "]")
      )

      if (any(!values_in_range)) {

        stop(
          "Detected", sum(!values_in_range), " element(s) of `x` that",
          " fall outside the expected range of ", range_label,  ".",
          call. = FALSE
        )

      }

    #* Finish

      ok

  }

  range_test <- function(
    x, lower = 0, upper = 1440,
    inc_lower = TRUE, inc_upper = FALSE
  ) {

    x %>%
    stats::na.omit(.) %>%
    {
      range_fun(
        ., lower, inc_lower,
        base::`>=`, base::`>`
      ) &
      range_fun(
        ., upper, inc_upper,
        base::`<=`, base::`<`
      )
    }

  }

  range_fun <- function(x, limit, include_limit, include_fun, exclude_fun) {
    if (include_limit) include_fun(x, limit) else exclude_fun(x, limit)
  }
