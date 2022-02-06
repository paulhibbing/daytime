testthat::test_that("daytime arithmetic works as expected", {

  x <- expand.grid(
    earlier = seq(0L, 1439L, 300L),
    later = seq(0L, 1439L, 300L)
  )

  earlier <- as_daytime(x$earlier)
  later <- as_daytime(x$later)

  testthat::expect_equal(
    daytime_modulo(earlier, later),
    c(0, 1140, 840, 540, 240, 300, 0, 1140, 840, 540, 600, 300, 0,
      1140, 840, 900, 600, 300, 0, 1140, 1200, 900, 600, 300, 0)
  )

  earlier <- as.POSIXct(tod(earlier), "UTC", format = "%H:%M:%S")
  later <- as.POSIXct(tod(later), "UTC", format = "%H:%M:%S")

  r1 <- as_daytime(later) - as_daytime(earlier)
  r2 <- as_daytime(earlier) - as_daytime(later)

  testthat::expect_s3_class(r1, "difftime")
  testthat::expect_equal(
    r1, structure(
      c(0, -300, -600, -900, -1200, 300, 0, -300, -600, -900,
        600, 300, 0, -300, -600, 900, 600, 300, 0, -300, 1200,
        900, 600, 300, 0),
      class = "difftime", units = "mins"
    )
  )

  testthat::expect_s3_class(r2, "difftime")
  testthat::expect_equal(
    r2, structure(
      c(0, -300, -600, -900, -1200, 300, 0, -300, -600, -900,
        600, 300, 0, -300, -600, 900, 600, 300, 0, -300, 1200,
        900, 600, 300, 0),
      class = "difftime", units = "mins"
    )*-1
  )

})
