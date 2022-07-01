#' @export
Ops.daytime <- function(e1, e2) {

  if (.Generic == "-") {
    switch(
      type_test(e1, e2),
      "POSIXt" = subtract_POSIXt(attr(e1, "x"), attr(e2, "x")),
      NextMethod() #default
    )
  } else {
    NextMethod()
  }

}

type_test <- function(e1, e2) {

  type <- "default"

  if (exists("x", attributes(e1)) & exists("x", attributes(e2))) {
    if (all(
      inherits(attr(e1, "x"), "POSIXt"),
      inherits(attr(e2, "x"), "POSIXt")
    )) type <- "POSIXt"
  }

  type

}

subtract_POSIXt <- function(e1, e2) {

  if (lubridate::tz(e1) != lubridate::tz(e2)) warning(
    "Performing arithmetic on daytime objects whose",
    " timezones differ", call. = FALSE
  )

  difftime(e1, e2, units = "mins")
  # subtract_daytime(attr(e1, "x"), attr(e2, "x"))

}
