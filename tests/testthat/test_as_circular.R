testthat::test_that("as_circular works as expected", {

  testthat::expect_error(
    as_circular(1440, FALSE),
    "outside the expected range.*1439]"
  )

  testthat::expect_error(
    as_circular(1440, TRUE),
    "outside the expected range.*1440)"
  )

  t1 <- 1439.9

  testthat::expect_equal(
    as_circular(t1, FALSE), floor(t1)/60, ignore_attr = TRUE
  )
  testthat::expect_equal(
    as_circular(t1, TRUE), t1/60, ignore_attr = TRUE
  )

  testthat::expect_equal(
    as_circular("05:45:30", FALSE, format = "%H:%M:%S"),
    345/60, ignore_attr = TRUE
  )

  testthat::expect_equal(
    as_circular("05:45:30", TRUE, format = "%H:%M:%S"),
    345.5/60, ignore_attr = TRUE
  )

  testthat::expect_s3_class(as_circular(Sys.time(), FALSE), "circular")

})
