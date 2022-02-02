testthat::test_that("as_daytime methods work as expected with (generally) `rational = FALSE`", {

  #* Character/POSIXt

    t1_char <- "00:01:30"
    t1 <- as.POSIXct(t1_char, "UTC", format = "%H:%M:%S")

    d1 <- as_daytime(t1)
    d2 <- as_daytime(t1_char, format = "%H:%M:%S")

    testthat::expect_equal(d1, 1, ignore_attr = TRUE)
    testthat::expect_s3_class(d1, "daytime")

    testthat::expect_equal(d2, 1, ignore_attr = TRUE)
    testthat::expect_s3_class(d2, "daytime")

  #* Numeric/Integer

    t2 <- 720.5
    t2_fake <- structure(t2, rational = TRUE)

    d3 <- as_daytime(t2, FALSE)

    testthat::expect_warning(as_daytime(t2), "^Setting `rational` to TRUE")
    testthat::expect_warning(as_daytime(floor(t2)), "^Setting `rational` to FALSE")

    suppressWarnings(testthat::expect_equal(as_daytime(t2), 720.5, ignore_attr = TRUE))
    suppressWarnings(testthat::expect_equal(as_daytime(floor(t2)), 720, ignore_attr = TRUE))

    testthat::expect_equal(d3, 720, ignore_attr = TRUE)
    testthat::expect_s3_class(d3, "daytime")

    testthat::expect_warning(as_daytime(t2_fake, FALSE), "^Conflict detected")
    suppressWarnings(testthat::expect_equal(
      as_daytime(t2_fake, FALSE), 720.5, ignore_attr = TRUE
    ))
    testthat::expect_equal(as_daytime(t2_fake, TRUE), 720.5, ignore_attr = TRUE)

    testthat::expect_error(
      as_daytime(t2, NA),
      "^isTRUE\\(is.logical\\(rational) & !is.na\\(rational))"
    )

    testthat::expect_error(as_daytime(1440, FALSE), "outside the expected range")
    testthat::expect_equal(as_daytime(1439.9, FALSE), 1439, ignore_attr = TRUE)

    testthat::expect_equal(as_daytime(5L), 5, ignore_attr = TRUE)

  #* Circular

    testthat::expect_error(
      as_daytime(circular::circular(t2)),
      "Expecting circular object with units==\"hours\""
    )

    testthat::expect_error(
      as_daytime(circular::circular(1300, units = "hours")),
      "Expecting circular object with all values in the interval \\[0, 24)"
    )

    testthat::expect_warning(
      as_daytime(circular::circular(1300.5/60, units = "hours"), FALSE),
      "Removing class `circular`"
    )

    suppressWarnings(testthat::expect_equal(
      as_daytime(circular::circular(1300.5/60, units = "hours"), FALSE),
      1300, ignore_attr = TRUE
    ))

    suppressWarnings(testthat::expect_s3_class(
      as_daytime(circular::circular(1300.5/60, units = "hours"), FALSE),
      "daytime"
    ))

})

testthat::test_that("as_daytime methods work as expected with (generally) `rational = FALSE`", {

  #* Character/POSIXt

    t1_char <- "00:01:30"
    t1 <- as.POSIXct(t1_char, "UTC", format = "%H:%M:%S")

    d1 <- as_daytime(t1, TRUE)
    d2 <- as_daytime(t1_char, TRUE, format = "%H:%M:%S")

    testthat::expect_equal(d1, 1.5, ignore_attr = TRUE)
    testthat::expect_s3_class(d1, "daytime")

    testthat::expect_equal(d2, 1.5, ignore_attr = TRUE)
    testthat::expect_s3_class(d2, "daytime")

  #* Numeric/Integer

    t2 <- 720.5
    t2_fake <- structure(t2, rational = FALSE)

    d3 <- as_daytime(t2, TRUE)

    testthat::expect_equal(d3, 720.5, ignore_attr = TRUE)
    testthat::expect_s3_class(d3, "daytime")

    testthat::expect_warning(as_daytime(t2_fake, TRUE), "^Conflict detected")
    suppressWarnings(testthat::expect_equal(
      as_daytime(t2_fake, FALSE), 720, ignore_attr = TRUE
    ))

    testthat::expect_equal(as_daytime(t2_fake, FALSE), 720, ignore_attr = TRUE)

    testthat::expect_equal(as_daytime(1439.9, TRUE), 1439.9, ignore_attr = TRUE)

    testthat::expect_equal(as_daytime(5L), 5, ignore_attr = TRUE)

  #* Circular

    testthat::expect_error(
      as_daytime(circular::circular(t2), TRUE),
      "Expecting circular object with units==\"hours\""
    )

    testthat::expect_error(
      as_daytime(circular::circular(1300, units = "hours"), TRUE),
      "Expecting circular object with all values in the interval \\[0, 24)"
    )

    testthat::expect_warning(
      as_daytime(circular::circular(1300.5/60, units = "hours"), TRUE),
      "Removing class `circular`"
    )

    suppressWarnings(testthat::expect_equal(
      as_daytime(circular::circular(1300.5/60, units = "hours"), TRUE),
      1300.5, ignore_attr = TRUE
    ))

    suppressWarnings(testthat::expect_s3_class(
      as_daytime(circular::circular(1300.5/60, units = "hours"), TRUE),
      "daytime"
    ))

})
