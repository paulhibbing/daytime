testthat::test_that("daytime methods work as expected", {

  x <- as_daytime(
    c("23:59:00", "23:59:59", "00:00:00", "00:00:59"),
    TRUE,
    format = "%H:%M:%S"
  )

  #* Mean/SD

    m <- mean(x)
    s <- sd(x)

    testthat::expect_s3_class(m, "daytime")
    testthat::expect_equal(
      m, 1439.992, tolerance = 0.001, ignore_attr = TRUE
    )

    testthat::expect_equal(tod(m), "23:59:59", ignore_attr = TRUE)
    testthat::expect_equal(s, 0.701239, ignore_attr = TRUE, tolerance = 0.0001)

  #* Subtraction

    y <- as_daytime(
      as.POSIXct(attr(x, "x"), "UTC", format = "%H:%M:%S")
    )

    testthat::expect_error(y - x, paste0(
      "`-` only works on daytime objects when both",
      " operands have a POSIXt `x` attribute"
    ))

    x <- as_daytime(
      lubridate::force_tz(attr(y, "x"), "EST")
    )

    testthat::expect_warning(
      x - y,
      "Performing arithmetic on daytime objects whose timezones differ"
    )

  #* c

    testthat::expect_equal(
      c(x, y), rep(c(1439, 1439, 0, 0), 2), ignore_attr = TRUE
    )

    testthat::expect_warning(
      c(x, y, as_daytime(816L)),
      "Converting all `x` attributes to character for concatenation"
    )

    testthat::expect_warning(
      c(as_daytime(816L), as_daytime(816, TRUE)),
      "Setting rational to TRUE"
    )

    testthat::expect_error(
      c(x, y, structure(20, rational = FALSE)),
      "all\\(sapply\\(., is.daytime)) is not TRUE"
    )

})
