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
    "Behavior is unknown when `daytime` object also inherits from",
    " `circular`.\nThere could be some coercion hazards"
  )



  pmax(x, 0) %>%
  pmin(1439) %>%
  {circular::circular(
    ./60, units = "hours", template = "clock24"
  )} %>%
  structure(
    x = attr(x, "x"),
    rational = attr(x, "rational")
  )

}
