# Generic -----------------------------------------------------------------

#' Cast an object as circular
#'
#' @param x object to cast
#' @param ... arguments passed to \code{\link{as_daytime}}
#'
#' @details For non-\code{daytime} objects, the default method is a chain of
#'   casts, first to \code{daytime}, then to \code{circular}. Extra arguments
#'   can be passed through \code{...} to assist with the initial conversion to
#'   \code{daytime}
#'
#' @keywords internal
as_circular <- function(x, ...) {
  UseMethod("as_circular", x)
}

# Methods -----------------------------------------------------------------

#' @export
as_circular.default <- function(x, ...) {
  as_daytime(x, ...) %>%
  as_circular.daytime(.)
}

#' @export
as_circular.daytime <- function(x, ...) {

  if (inherits(x, "circular")) stop(
    "`daytime` object should not also inherit from",
    " `circular` -- coercion could\nbe hazardous due",
    " to previous adjustments for `first_min` followed",
    " by loss of the attribute"
  )

  if (attr(x, "first_min") == 1) {
    x %<>% {. - 1}
  }

  if (!range_test(x, 0)) {

    if (!isTRUE(attr(x, "rational"))) stop(
      "Unknown reason for failing range_test in as_circular.daytime",
      call. = FALSE
    )

    warning(
      "Detected element(s) of `x` that",
      " fall outside the expected range of [0,1439].\nThey will be",
      " rounded into that range. Avoid this warning by setting",
      " `rational=FALSE`\nin the original call to `as_daytime`",
      call. = FALSE
    )

  }

  pmax(x, 0) %>%
  pmin(1439) %>%
  {circular::circular(
    ./60, units = "hours", template = "clock24"
  )} %>%
  structure(
    x = attr(x, "x"),
    rational = attr(x, "rational"),
    first_min = attr(x, "first_min")
  )

}
