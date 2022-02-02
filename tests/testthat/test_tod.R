testthat::test_that("`tod` works as expected", {

  #* Character/POSIX

    testthat::expect_equal(
      tod("05:30:30", FALSE, format = "%H:%M:%S"),
      "05:30:00", ignore_attr = TRUE
    )

    testthat::expect_equal(
      tod("05:30:30", TRUE, format = "%H:%M:%S"),
      "05:30:30", ignore_attr = TRUE
    )

    testthat::expect_true(
      is.character(tod(Sys.time(), TRUE))
    )

    testthat::expect_true(
      grepl(":00$", tod(Sys.time(), FALSE))
    )

  #* Numeric/Integer

    t2 <- 720.5
    t2_fake <- structure(t2, rational = TRUE)

    testthat::expect_warning(tod(t2), "Setting `rational` to TRUE")
    testthat::expect_warning(tod(floor(t2)), "Setting `rational` to FALSE")

    testthat::expect_equal(tod(t2, TRUE), "12:00:30", ignore_attr = TRUE)
    testthat::expect_equal(tod(t2, FALSE), "12:00:00", ignore_attr = TRUE)

    testthat::expect_warning(tod(t2_fake, FALSE), "^Conflict detected")
    suppressWarnings(testthat::expect_equal(
      tod(t2_fake, FALSE), "12:00:30", ignore_attr = TRUE
    ))
    testthat::expect_equal(tod(t2_fake, TRUE), "12:00:30", ignore_attr = TRUE)

    testthat::expect_error(
      tod(t2, NA),
      "^isTRUE\\(is.logical\\(rational) & !is.na\\(rational))"
    )

    testthat::expect_error(tod(1440, FALSE), "outside the expected range")
    testthat::expect_equal(tod(1439.999, FALSE), "23:59:00", ignore_attr = TRUE)
    testthat::expect_equal(tod(1439.9999, TRUE), "23:59:59", ignore_attr = TRUE)

    testthat::expect_equal(tod(5L), "00:05:00", ignore_attr = TRUE)

  #* Circular

    suppressWarnings(testthat::expect_error(
      tod(circular::circular(t2)),
      "Expecting circular object with units==\"hours\""
    ))

    testthat::expect_warning(
      try(tod(circular::circular(t2)), TRUE),
      "Setting `rational` to TRUE"
    )

    suppressWarnings(testthat::expect_error(
      tod(circular::circular(1300, units = "hours")),
      "Expecting circular object with all values in the interval \\[0, 24)"
    ))

    testthat::expect_warning(
      try(tod(circular::circular(1300, units = "hours")), TRUE),
      "Setting `rational` to FALSE"
    )

    testthat::expect_warning(
      tod(circular::circular(1300.5/60, units = "hours"), FALSE),
      "Removing class `circular`"
    )

    suppressWarnings(testthat::expect_equal(
      tod(circular::circular(1300.5/60, units = "hours"), FALSE),
      "21:40:00", ignore_attr = TRUE
    ))

    suppressWarnings(testthat::expect_equal(
      tod(circular::circular(1300.5/60, units = "hours"), TRUE),
      "21:40:30", ignore_attr = TRUE
    ))

})
