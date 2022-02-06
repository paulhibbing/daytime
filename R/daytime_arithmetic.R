#' Compare the modulo distance between two \code{daytime} objects
#'
#' @param earlier the earlier of the objects
#' @param later the later of the objects
#'
#' @details There is quite a bit of variability in how \code{daytime} objects
#'   look. Therefore, it is an elusive goal to develop a straightforward
#'   subtraction method for determining the time lag between them (although you
#'   can try \code{\link{subtract_daytime}}). This function relies on \emph{a
#'   priori} knowledge of which object occurred earlier and which occurred
#'   later. It moves leftward around the circle from the later object to the
#'   earlier, recording the number of intervening minutes. But of course,
#'   \code{daytime} objects do not explicitly reflect the actual day on which
#'   the time occurred, so there is no reflection of the number of revolutions
#'   involved in actually getting from one timestamp to the next. Instead, the
#'   assumption is that they are separated by no more than a day (i.e., the
#'   difference will always fall in the interval \code{[0,1440)}). In other
#'   words, this is not a true difference, only a modulo (hence the function
#'   name). This is directly analogous to the fact that various angles can all
#'   lie on the same spot of a circle (e.g., \eqn{2\pi}, \eqn{4\pi}, \eqn{6\pi},
#'   etc). Since we don't know the true coefficient for our circumference, we
#'   return the modulo.
#'
#' @export
#'
#' @examples
#' t1 <- as_daytime(1000L)
#' t2 <- as_daytime(1300L)
#'
#' daytime_modulo(t1, t2) ## 300
#' daytime_modulo(t2, t1) ## 1140
daytime_modulo <- function(earlier, later) {

  stopifnot(
    is.daytime(earlier),
    is.daytime(later)
  )

  {as.numeric(later) + 1440 - as.numeric(earlier)} %>%
  {. %% 1440} %T>%
  {stopifnot(range_test(., 0, 1440, rational_adjust = FALSE))}

}

#' Perform subtraction on \code{daytime} objects
#'
#' @param e1 a POSIX-formatted \code{x} attribute from a \code{daytime} object
#' @param e2 a POSIX-formatted \code{x} attribute from a \code{daytime} object
#'
#' @return a \code{difftime} object, in minutes
#'
#' @details This is a decidedly internal function that is strictly a wrapper
#'   around \code{difftime}. The magic happens with \code{Ops} dispatch of the
#'   \code{`-`} operator, where the objects are checked and the original
#'   timestamps pulled out.
#'
#' @seealso \code{daytime:::Ops.daytime}
#'
#' @examples
#' base_time <- Sys.time()
#' t1 <- as_daytime(base_time)
#' t2 <- as_daytime(base_time + 86460)
#'
#' t1 - t2
#' t2 - t1
#'
#' @keywords internal
subtract_daytime <- function(e1, e2) {

  difftime(e1, e2, units = "mins")

}
