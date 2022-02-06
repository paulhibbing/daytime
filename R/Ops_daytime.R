#' @export
Ops.daytime <- function(e1, e2) {

  if (.Generic == "-") {
    daytime_check_POSIXt(attr(e1, "x"), attr(e2, "x"), .Generic)
    subtract_daytime(attr(e1, "x"), attr(e2, "x"))
  } else {
    NextMethod()
  }

}

daytime_check_POSIXt <- function(e1, e2, f) {

  if (!(inherits(e1, "POSIXt") & inherits(e2, "POSIXt"))) stop(
    gettextf(
      c("`%s` only works on daytime objects when both",
      " operands have a POSIXt `x` attribute"),
      f
    ),
    call. = FALSE
  )

  if (lubridate::tz(e1) != lubridate::tz(e2)) warning(
    "Performing arithmetic on daytime objects whose",
    " timezones differ", call. = FALSE
  )

  invisible()

}
