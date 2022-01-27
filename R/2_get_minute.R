# Internal helpers --------------------------------------------------------

  check_time <- function (timestamp, ...) {
    timestamp %T>%
    {stopifnot(inherits(., c("character", "POSIXt")))} %>%
    {if (is.character(.)) as.POSIXct(., ...) else .}
  }

# Primary function(s) -----------------------------------------------------

#' Convert timestamp to minute of day
#'
#' @param timestamp timestamp input, can inherit from \code{POSIXt} or
#'   \code{character}, and can be either scalar or vector
#' @inheritParams as.daytime
#'
#' @return Values formatted as minute of day
#' @export
#'
#' @examples
#' Time <- Sys.time()
#' Time
#' get_minute(Time)
#' get_minute(Time, TRUE)
#' get_minute(Time, TRUE, 1)
get_minute <- function(timestamp, rational = FALSE, first_min = 0, ...) {

  stopifnot(first_min %in% 0:1)

  check_time(timestamp, ...) %>%
  {. - lubridate::floor_date(., "days")} %>%
  as.numeric("mins") %>%
  {. - ((. - floor(.)) * !rational)} %>%
  {. + first_min}

}
