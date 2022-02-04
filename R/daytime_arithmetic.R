#' Compare the modulo distance between two \code{daytime} objects
#'
#' @param earlier the earlier of the objects
#' @param later the later of the objects
#'
#' @details There is quite a bit of variability in how \code{daytime} objects
#'   look. Therefore, it is an elusive goal to develop a straightforward
#'   subtraction method for determining the time lag between them. This function
#'   relies on \emph{a priori} knowledge of which object occurred earlier and
#'   which occurred later. It moves leftward around the circle from the later
#'   object to the earlier, recording the number of intervening minutes. But of
#'   course, \code{daytime} objects do not explicitly reflect the actual day on
#'   which the time occurred, so there is no reflection of the number of
#'   revolutions involved in actually getting from one timestamp to the next.
#'   Instead, the assumption is that they are separated by no more than a day
#'   (i.e., the difference will always fall in the interval \code{[0,1440)}).
#'   In other words, this is not a true difference, only a modulo (hence the
#'   function name). This is directly analogous to the fact that various angles
#'   can all lie on the same spot of a circle (e.g., \eqn{2\pi}, \eqn{4\pi},
#'   \eqn{6\pi}, etc). Since we don't know the true coefficient for our
#'   circumference, we return the modulo.
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

  {later + 1440 - earlier} %>%
  {. %%1440} %>%
  as.numeric(.) %T>%
  {stopifnot(range_test(., 0, 1440, rational_adjust = FALSE))}

}

# subtract_daytime <- function(e1, e2) {
#
#   stopifnot(is.daytime(e1), is.daytime(e2))
#
#   x1 <- attr(e1, "x")
#   x2 <- attr(e2, "x")
#
#   if (!(inherits(x1, "POSIXt") & inherits(x2, "POSIXt"))) stop(
#     "Subtraction of daytime objects only works when both objects ",
#     "have an `x` attribute that inherits from POSIXt", call. = FALSE
#   )
#
#   timezone <-
#     lubridate::tz(x1) %T>%
#     {stopifnot(. == lubridate::tz(x2))}
#
#   difftime(x1, x2, timezone) %>%
#   as.numeric("mins")
#
# }
