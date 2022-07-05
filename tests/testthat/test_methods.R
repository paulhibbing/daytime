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
    testthat::expect_equal(s, 0.5, ignore_attr = TRUE, tolerance = 0.0001)

  #* Subtraction

    diffs <- c(75, 42, 19, 90)

    y <- as_daytime(
      as.POSIXct(attr(x, "x"), "UTC", format = "%H:%M:%S") + diffs
    )

    x <- as_daytime(
      as.POSIXct(attr(x, "x"), "EST", format = "%H:%M:%S")
    )

    testthat::expect_warning(
      x - y,
      "Performing arithmetic on daytime objects whose timezones differ"
    )

    x <- as_daytime(
      lubridate::force_tz(attr(x, "x"), lubridate::tz(attr(y, "x")))
    )

    result <- y - x
    testthat::expect_s3_class(result, "difftime")
    testthat::expect_equal(as.numeric(result, "secs"), diffs)
    testthat::expect_equal(as.numeric(x - y, "secs"), -diffs)

  #* c

    testthat::expect_equal(
      c(x, y), c(1439, 1439, rep(0, 5), 2), ignore_attr = TRUE
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
