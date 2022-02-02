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

  } else if (is.null(rational) & !inherits(x, "POSIXt")) {

    if (is.integer(x)) {
      rational <- FALSE
    } else {
      rational <- !isTRUE(all.equal(as.numeric(x), as.integer(x)))
      warning("Setting `rational` to ", rational, call. = FALSE)
    }

  } else if (is.null(rational) & inherits(x, "POSIXt")) {

    warning("Setting `rational` to FALSE", call. = FALSE)
    rational <- FALSE

  } else {

    stopifnot(isTRUE(is.logical(rational) & !is.na(rational)))

  }

  rational

}
